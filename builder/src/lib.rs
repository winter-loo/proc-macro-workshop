use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    // eprintln!("AST is {:#?}", ast);
    impl_builder(&ast)
}

fn impl_builder(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let mut setters = std::vec::Vec::new();
    let mut fields = std::vec::Vec::new();
    let mut values = std::vec::Vec::new();
    let mut initializers = std::vec::Vec::new();

    match &ast.data {
        syn::Data::Struct(data) => {
            for field in data.fields.iter() {
                let field_name = field.ident.as_ref().unwrap();
                let field_type = &field.ty;
                let leading_type = get_leading_typename(field_type);
                add_setter(field, &mut setters);
                let f = quote! {
                    #field_name: std::option::Option<#field_type>,
                };
                let i = quote! {
                    #field_name: std::option::Option::None,
                };
                fields.push(f);
                initializers.push(i);

                values.push(quote! {
                    #field_name: match self.#field_name.take() {
                        std::option::Option::Some(v) => v,
                        std::option::Option::None => #leading_type::default(),
                    },
                });
            }
        }
        _ => unimplemented!(),
    };

    let builder_name = format_ident!("{}Builder", name);
    let gen = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#initializers)*
                }
            }
        }

        pub struct #builder_name {
            #(#fields)*
        }

        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#values)*
                })
            }
        }
    };
    gen.into()
}

fn get_leading_typename(ty: &syn::Type) -> &syn::Ident {
    match ty {
        syn::Type::Path(t) => {
            // eprintln!("Type path is {:#?}", t);
            for p in t.path.segments.pairs() {
                let seg = p.into_value();
                // eprintln!("- Segment is {:#?}", seg);
                if !seg.arguments.is_none() {
                    return &seg.ident;
                }
            }
            let seg = t.path.segments.first().unwrap();
            &seg.ident
        }
        _ => unimplemented!(),
    }
}

fn get_element_type(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Path(t) => {
            // eprintln!("Type path is {:#?}", t);
            for p in t.path.segments.pairs() {
                let seg = p.into_value();
                // eprintln!("- Segment is {:#?}", seg);
                match seg.arguments {
                    syn::PathArguments::AngleBracketed(ref angle) => {
                        let arg = angle.args.iter().next().unwrap();
                        match arg {
                            syn::GenericArgument::Type(ty) => {
                                return get_leading_typename(ty).to_string();
                            }
                            _ => unimplemented!(),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            let seg = t.path.segments.first().unwrap();
            seg.ident.to_string()
        }
        _ => unimplemented!(),
    }
}

fn add_setter(field: &syn::Field, setters: &mut std::vec::Vec<proc_macro2::TokenStream>) {
    let field_name = field.ident.as_ref().unwrap();
    let field_type = &field.ty;
    let mut has_same_name = false;

    for attr in field.attrs.iter() {
        let mut setter_name = String::from("");
        let mut element_type = String::from("");
        let mut has_error = false;
        // parse #[builder(each = "arg")]
        if attr.path().is_ident("builder") {
            match attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("each") {
                    let fldtyp = get_leading_typename(&field.ty);
                    if fldtyp != "Vec" {
                        return Err(meta.error("each attribute only works with Vec"));
                    }
                    element_type = get_element_type(&field.ty);

                    let value = meta.value().unwrap();
                    let s: syn::LitStr = value.parse().unwrap();
                    setter_name = s.value();
                    if setter_name == field_name.to_string() {
                        has_same_name = true;
                    }
                    return Ok(());
                }
                Err(meta.error("expected `builder(each = \"...\")`"))
            }) {
                Ok(_) => (),
                Err(e) => {
                    setters.push(e.to_compile_error());
                    has_error = true;
                }
            }
            if !has_error {
                let setter_name = format_ident!("{}", setter_name);
                let element_type = format_ident!("{}", element_type);
                setters.push(quote! {
                    pub fn #setter_name(&mut self, a: #element_type) -> &mut Self {
                        if self.#field_name.is_none() {
                            self.#field_name = std::option::Option::Some(std::vec::Vec::new());
                        }
                        self.#field_name.as_mut().unwrap().push(a);
                        self
                    }
                });
            }
        }
    }

    if !has_same_name {
        setters.push(quote! {
            pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                self.#field_name = std::option::Option::Some(#field_name);
                self
            }
        });
    }
}
