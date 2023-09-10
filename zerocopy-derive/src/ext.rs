// Copyright 2019 The Fuchsia Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{Data, DataEnum, DataStruct, DataUnion, Field, Fields, Index, Type};

pub trait DataExt {
    /// Extract the names and types of all fields. For enums, extract the names
    /// and types of fields from each variant. For tuple structs, the names are
    /// the indices used to index into the struct (ie, `0`, `1`, etc).
    ///
    /// TODO: Extracting field names for enums doesn't really make sense. Types
    /// makes sense because we don't care about where they live - we just care
    /// about transitive ownership. But for field names, we'd only use them when
    /// generating is_bit_valid, which cares about where they live.
    fn fields(&self) -> Vec<(TokenStream, &Type)>;
}

impl DataExt for Data {
    fn fields(&self) -> Vec<(TokenStream, &Type)> {
        match self {
            Data::Struct(strc) => strc.fields(),
            Data::Enum(enm) => enm.fields(),
            Data::Union(un) => un.fields(),
        }
    }
}

impl DataExt for DataStruct {
    fn fields(&self) -> Vec<(TokenStream, &Type)> {
        map_fields(&self.fields)
    }
}

impl DataExt for DataEnum {
    fn fields(&self) -> Vec<(TokenStream, &Type)> {
        map_fields(self.variants.iter().flat_map(|var| &var.fields))
    }
}

impl DataExt for DataUnion {
    fn fields(&self) -> Vec<(TokenStream, &Type)> {
        map_fields(&self.fields.named)
    }
}

impl DataExt for Fields {
    fn fields(&self) -> Vec<(TokenStream, &Type)> {
        map_fields(self)
    }
}

fn map_fields<'a>(
    fields: impl 'a + IntoIterator<Item = &'a Field>,
) -> Vec<(TokenStream, &'a Type)> {
    fields
        .into_iter()
        .enumerate()
        .map(|(idx, f)| {
            (
                f.ident
                    .as_ref()
                    .map(ToTokens::to_token_stream)
                    .unwrap_or_else(|| Index::from(idx).to_token_stream()),
                &f.ty,
            )
        })
        .collect()
}

pub trait EnumExt {
    fn is_c_like(&self) -> bool;
}

impl EnumExt for DataEnum {
    fn is_c_like(&self) -> bool {
        self.fields().is_empty()
    }
}
