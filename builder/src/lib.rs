use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenTree};
use syn::{Error, Result, Data, DeriveInput, Fields, parse_macro_input, Type, PathArguments, GenericArgument, Attribute, Meta};
use quote::quote;
use syn::spanned::Spanned;
use regex::Regex;


fn check_segment_ident(target: &str, field_type: &Type) ->bool{
    if let Type::Path(type_path) = &field_type{
        let segments = &type_path.path.segments;
        for segment in segments{
            if segment.ident == target{
                return true;
            }
        }
    }
    false
}

fn get_inner_type(field_type: &Type) ->Option<GenericArgument>{
    if let Type::Path(type_path) = &field_type{
        let segments = &type_path.path.segments;
        for segment in segments{
            if let PathArguments::AngleBracketed(bracketed) = &segment.arguments{
                if let Some(first_args) = bracketed.args.first(){
                    return Some(first_args.clone());
                }
            }
        }
    }
    None
}

fn get_attr_value(attributes: &Vec<Attribute>) -> Result<Option<Ident>>{
    for attr in attributes{
        if let Meta::List(metadata) = &attr.meta{
            if metadata.path.is_ident("builder"){
                let tokens = &metadata.tokens;
                let tokentree = &tokens.clone().into_iter().map(|token| token).collect::<Vec<_>>();
                if tokentree.len() != 3 { continue;}
                if let TokenTree::Ident(first) = &tokentree[0]{
                    if let TokenTree::Literal(last) = &tokentree[2]{
                        if first.to_string() == "each"{
                            let re = Regex::new(r#""([^"]*)""#).unwrap();
                            return match re.captures(last.to_string().as_str()) {
                                Some(caps) => {
                                    let word = caps.get(1).map_or("", |m| m.as_str());
                                    Ok(Some(Ident::new(word, first.span())))
                                },
                                _ => Ok(None)
                            };
                        } else{
                            return Err(Error::new(metadata.span(), "expected `builder(each = \"...\")`"))
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    let content = if let Data::Struct(struct_data) = &input.data{
        if let Fields::Named(field) = &struct_data.fields{
            let builder_struct_init = field.named.iter().map(|f|{
                let field_name = &f.ident;
                let field_type = &f.ty;
                if check_segment_ident("Vec", field_type){
                    quote!(
                        #field_name: vec![],
                    )
                } else{
                    quote!(
                        #field_name: None,
                    )
                }
            }).collect::<Vec<_>>();


            let builder_struct_attribute = field.named.iter().map(|f|{
                let field_name = &f.ident;
                let field_type = &f.ty;

                // 만일, field_type이 Option<T>라면 Option을 추가로 감싸 줄 필요가 없음.
                // field_type가 Vec이면 vec![]로 초기화 한다. 그리고 Type 또한, Option 형으로 두지 않음.

                if check_segment_ident("Option", field_type) || check_segment_ident("Vec", field_type){
                    quote!{
                        #field_name: #field_type,
                    }
                } else{
                    quote!{
                        #field_name: std::option::Option<#field_type>,
                    }
                }
            }).collect::<Vec<_>>();

            let setter_content = field.named.iter().map(|f|{
                let field_name = &f.ident;
                let field_type = &f.ty;
                let option_flag = check_segment_ident("Option", field_type);
                let single_item = get_attr_value(&f.attrs);

                match single_item{
                    Ok(single_item_ident) => {
                        match single_item_ident{
                            Some(single_item)=>{
                                let option_type = get_inner_type(field_type).unwrap();
                                quote!(
                            fn #single_item(&mut self, #field_name: #option_type)->&mut Self{
                                self.#field_name.push(#field_name);
                                self
                            }
                        )
                            },
                            _=> match option_flag{
                                true=>{
                                    let option_type = get_inner_type(field_type).unwrap();
                                    quote!(
                                fn #field_name(&mut self, #field_name: #option_type)->&mut Self{
                                    self.#field_name = Some(#field_name);
                                    self
                                }
                            )
                                },
                                false=>{
                                    match check_segment_ident("Vec", field_type){
                                        true=> quote!(
                                    fn #field_name(&mut self, #field_name: #field_type)->&mut Self{
                                        self.#field_name = #field_name;
                                        self
                                    }
                                ),
                                        false=>quote!(
                                    fn #field_name(&mut self, #field_name: #field_type)->&mut Self{
                                        self.#field_name = Some(#field_name);
                                        self
                                    }
                                )
                                    }

                                }
                            }
                        }
                    },
                    Err(err)=>{
                        err.to_compile_error()
                    }
                }
            }).collect::<Vec<_>>();

            let build_content1 = field.named.iter().map(|f|{
                let field_name = &f.ident;
                let field_type = &f.ty;

                if check_segment_ident("Vec", field_type){
                    quote!(
                        let #field_name = self.#field_name.clone();
                    )
                } else if check_segment_ident("Option", field_type){
                    quote!(
                        let #field_name = self.#field_name.take();
                    )
                } else{
                    quote!(
                        let #field_name = self.#field_name.as_ref()
                        .ok_or_else(|| std::boxed::Box::<dyn Error>::from("#field_name should not be none."))?
                        .clone();
                    )
                }
            }).collect::<Vec<_>>();

            let build_content2 = field.named.iter().map(|f|{
                let field_name = &f.ident;
                quote!(
                    #field_name,
                )
            }).collect::<Vec<_>>();

            let build = quote!(
                pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn Error>>{
                    #(#build_content1)*

                    Ok(
                        #name{
                            #(#build_content2)*
                        }
                    )
                }
            );

            (builder_struct_init, builder_struct_attribute, setter_content, build)

        } else{
            panic!("field should be named.")
        }
    }else{
        panic!("builder should be used in struct");
    };

    let builder_struct = Ident::new(&format!("{}Builder", name), Span::call_site());


    let builder_impl_content = content.0;
    let new_struct_content = content.1;
    let setter_content = content.2;
    let build_content = content.3;

    let builder_impl = quote!(
        impl #name{
            pub fn builder()->#builder_struct{
                #builder_struct{
                    #(#builder_impl_content)*
                }
            }
        }
    );

    let new_struct = quote!(
        pub struct #builder_struct{
            #(#new_struct_content)*
        }
    );

    let builder_struct_setter = quote!(
        impl #builder_struct{
            #(#setter_content)*

            #build_content
        }
    );


    let new_quote = quote!(
        use std::error::Error;

        #new_struct

        #builder_impl

        #builder_struct_setter
    );
    TokenStream::from(new_quote)
}