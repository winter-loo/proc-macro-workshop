use proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_custom_debug(&ast)
}

fn impl_custom_debug(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &ast.data
    {
        named
    } else {
        unimplemented!();
    };

    // eprintln!("generics: {:#?}", ast.generics);
    let generics_typnames = get_generics_typnames(&ast.generics);
    // eprintln!("generics_typnames: {:#?}", generics_typnames);
    let mut generics_typnames = generics_typnames
        .iter()
        .filter(|&name| {
            let mut only_used_in_phantom = true;
            for field in fields {
                if let syn::Type::Path(t) = &field.ty {
                    for p in t.path.segments.pairs() {
                        let seg = p.into_value();
                        let is_phantomed = seg.ident.to_string() == "PhantomData";
                        let included = name == &seg.ident.to_string()
                            || is_in_path_arguments(name, &seg.arguments);
                        if dbg!(included) && dbg!(!is_phantomed) {
                            only_used_in_phantom = false;
                        }
                    }
                }
            }
            !only_used_in_phantom
        })
        .cloned()
        .collect::<Vec<_>>();

    let mut associates = Vec::new();
    generics_typnames.iter().for_each(|name| {
        let mut asso = false;
        let (mut firtyp, mut sectyp) = (String::new(), String::new());
        for field in fields {
            if let syn::Type::Path(t) = &field.ty {
                if let Some(tp) = get_associated_type(&t, name) {
                    asso = true;
                    firtyp = tp.0;
                    sectyp = tp.1;
                } else {
                    asso = false;
                    break;
                }
            }
        }
        if asso {
            associates.push((firtyp, sectyp));
        }
    });
    // eprintln!("asso: {:#?}", associates);

    let mut compile_error = None;
    let mut where_bounds = Vec::new();
    ast.attrs.iter().for_each(|attr| {
        if let Err(e) = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("bound") {
                let value = meta.value().unwrap();
                let s: syn::LitStr = value.parse().unwrap();
                where_bounds.push(s.value());
                return Ok(());
            }
            Err(meta.error("expected `debug(bound = \"...\")`"))
        }) {
            compile_error = Some(e.to_compile_error());
        }
    });

    // eprintln!("generics_typnames2: {:#?}", generics_typnames);
    let generics = add_trait_bounds(
        ast.generics.clone(),
        &mut generics_typnames,
        &mut associates,
        &mut where_bounds,
        &mut compile_error,
    );
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let debug_fields = fields
        .iter()
        .filter(|field| get_leading_typename(&field.ty) != "PhantomData")
        .map(|field| {
            let field_name = field.ident.as_ref().unwrap();
            let mut debug_format = String::new();
            field.attrs.iter().for_each(|attr| {
                if let syn::Meta::NameValue(name_value) = &attr.meta {
                    let name = name_value.path.segments.first().unwrap().ident.to_string();
                    if name == "debug" {
                        if let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(lit_str),
                            ..
                        }) = &name_value.value
                        {
                            debug_format = lit_str.value();
                        } else {
                            unimplemented!();
                        }
                    } else {
                        unimplemented!();
                    }
                } else {
                    unimplemented!();
                }
            });

            if debug_format.is_empty() {
                quote! {
                    .field(stringify!(#field_name), &self.#field_name)
                }
            } else {
                quote! {
                    .field(stringify!(#field_name), &format_args!(#debug_format, &self.#field_name))
                }
            }
        });

    let gen = quote! {
        #compile_error
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                    #(#debug_fields)*
                    .finish()
            }
        }
    };
    gen.into()
}

fn add_trait_bounds(
    mut generics: syn::Generics,
    included: &mut Vec<String>,
    associates: &mut Vec<(String, String)>,
    where_bounds: &mut Vec<String>,
    compile_error: &mut Option<proc_macro2::TokenStream>,
) -> syn::Generics {
    where_bounds.iter().for_each(
        |bound| match syn::parse_str::<syn::WherePredicate>(&bound) {
            Ok(pred) => {
                if let syn::WherePredicate::Type(syn::PredicateType { bounded_ty: ty, .. }) = &pred
                {
                    let name = get_leading_typename(&ty);
                    included.retain(|n| n != &name);
                    if let syn::Type::Path(tpath) = &ty {
                        if let Some(tp) = get_associated_type(&tpath, &name) {
                            associates
                                .retain(|(firtyp, sectyp)| firtyp != &tp.0 && sectyp != &tp.1);
                        }
                    }
                    generics.make_where_clause().predicates.push(pred);
                }
            }
            Err(e) => {
                let e = syn::Error::new(e.span(), "invalid bound");
                *compile_error = Some(e.to_compile_error());
            }
        },
    );
    associates.iter().for_each(|(firtyp, sectyp)| {
        let name1 = quote::format_ident!("{}", firtyp);
        let name2 = quote::format_ident!("{}", sectyp);
        generics
            .make_where_clause()
            .predicates
            .push(syn::parse_quote!(#name1::#name2: std::fmt::Debug));
    });
    for param in &mut generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            let generic_typname = type_param.ident.to_string();
            if associates
                .iter()
                .all(|(firtyp, _)| &generic_typname != firtyp)
            {
                if included.contains(&generic_typname) {
                    type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
                }
            }
        }
    }
    // eprintln!("where clause: {:#?}", generics.where_clause);
    generics
}

fn get_leading_typename(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Path(t) => {
            // eprintln!("Type path is {:#?}", t);
            for p in t.path.segments.pairs() {
                let seg = p.into_value();
                // eprintln!("- Segment is {:#?}", seg);
                if !seg.arguments.is_none() {
                    return seg.ident.to_string();
                }
            }
            let seg = t.path.segments.first().unwrap();
            seg.ident.to_string()
        }
        _ => String::new(),
    }
}

fn get_generics_typnames(generics: &syn::Generics) -> Vec<String> {
    let mut typnames = Vec::new();
    for param in &generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            typnames.push(type_param.ident.to_string());
        }
    }
    typnames
}

fn is_in_path_arguments(name: &str, arguments: &syn::PathArguments) -> bool {
    match arguments {
        syn::PathArguments::None => false,
        syn::PathArguments::AngleBracketed(angle) => {
            for arg in angle.args.iter() {
                if let syn::GenericArgument::Type(ty) = arg {
                    if let syn::Type::Path(t) = ty {
                        for p in t.path.segments.pairs() {
                            let seg = p.into_value();
                            if seg.ident == name {
                                return true;
                            } else {
                                if is_in_path_arguments(name, &seg.arguments) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            false
        }
        syn::PathArguments::Parenthesized(_) => false,
    }
}

fn get_associated_type(tpath: &syn::TypePath, name: &str) -> Option<(String, String)> {
    let pthsegs = &tpath.path.segments;
    if pthsegs.len() == 2 {
        let it = &mut pthsegs.iter();
        let seg1 = it.next().unwrap();
        let seg2 = it.next().unwrap();
        if seg1.ident == name
            && seg1.arguments == syn::PathArguments::None
            && seg2.arguments == syn::PathArguments::None
        {
            Some((seg1.ident.to_string(), seg2.ident.to_string()))
        } else {
            None
        }
    } else {
        if let Some(syn::PathSegment {
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
            ..
        }) = pthsegs.first()
        {
            for arg in args {
                if let syn::GenericArgument::Type(syn::Type::Path(ty)) = arg {
                    if let Some(ans) = get_associated_type(&ty, name) {
                        return Some(ans);
                    }
                }
            }
            return None;
        }
        None
    }
}
