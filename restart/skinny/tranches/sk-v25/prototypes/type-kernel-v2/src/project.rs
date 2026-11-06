use crate::{ProjectError, Solution, SolvedRow, SolvedTail, SolvedType, TypeId, TypeStore};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Projection {
    name: String,
    semantic: SolvedType,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratedArtifacts {
    rust: String,
    typescript: String,
}

impl GeneratedArtifacts {
    pub fn rust(&self) -> &str {
        &self.rust
    }

    pub fn typescript(&self) -> &str {
        &self.typescript
    }
}

impl Projection {
    pub fn from_solution(
        name: impl Into<String>,
        solution: &Solution,
        store: &TypeStore,
        root: TypeId,
    ) -> Result<Self, ProjectError> {
        let name = name.into();
        if !valid_name(&name) {
            return Err(ProjectError::InvalidName);
        }
        let semantic = solution
            .resolve(store, root)
            .map_err(|_| ProjectError::Unresolved("solution/store mismatch".into()))?;
        Ok(Self { name, semantic })
    }

    pub fn generate(&self) -> Result<GeneratedArtifacts, ProjectError> {
        Ok(GeneratedArtifacts {
            rust: generate_rust(&self.name, &self.semantic)?,
            typescript: generate_typescript(&self.name, &self.semantic)?,
        })
    }
}

fn valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|first| first == '_' || first.is_ascii_alphabetic())
        && chars.all(|character| character == '_' || character.is_ascii_alphanumeric())
}

fn generate_rust(name: &str, ty: &SolvedType) -> Result<String, ProjectError> {
    match ty {
        SolvedType::Record(row) => {
            ensure_closed(row)?;
            let fields = row
                .fields
                .iter()
                .map(|(label, ty)| Ok(format!("    pub {label}: {},", rust_type(ty)?)))
                .collect::<Result<Vec<_>, ProjectError>>()?
                .join("\n");
            Ok(format!(
                "// @generated from solved semantic type; do not edit.\n#[derive(Clone, Debug, PartialEq)]\npub struct {name} {{\n{fields}\n}}\n"
            ))
        }
        SolvedType::Variant(row) => {
            ensure_closed(row)?;
            let variants = row
                .fields
                .iter()
                .map(|(label, ty)| {
                    let label = upper_camel(label)?;
                    Ok(if *ty == SolvedType::Primitive(crate::Primitive::Unit) {
                        format!("    {label},")
                    } else {
                        format!("    {label}({}),", rust_type(ty)?)
                    })
                })
                .collect::<Result<Vec<_>, ProjectError>>()?
                .join("\n");
            Ok(format!(
                "// @generated from solved semantic type; do not edit.\n#[derive(Clone, Debug, PartialEq)]\npub enum {name} {{\n{variants}\n}}\n"
            ))
        }
        _ => Ok(format!(
            "// @generated from solved semantic type; do not edit.\npub type {name} = {};\n",
            rust_type(ty)?
        )),
    }
}

fn generate_typescript(name: &str, ty: &SolvedType) -> Result<String, ProjectError> {
    Ok(format!(
        "// @generated from solved semantic type; do not edit.\nexport type {name} = {};\n",
        typescript_type(ty)?
    ))
}

fn rust_type(ty: &SolvedType) -> Result<String, ProjectError> {
    Ok(match ty {
        SolvedType::Meta(_) | SolvedType::Bound(_) => {
            return Err(ProjectError::Unresolved(ty.render()));
        }
        SolvedType::Primitive(crate::Primitive::Bool) => "bool".into(),
        SolvedType::Primitive(crate::Primitive::Bytes) => "Vec<u8>".into(),
        SolvedType::Primitive(crate::Primitive::F64) => "f64".into(),
        SolvedType::Primitive(crate::Primitive::I64) => "i64".into(),
        SolvedType::Primitive(crate::Primitive::Str) => "String".into(),
        SolvedType::Primitive(crate::Primitive::Unit) => "()".into(),
        SolvedType::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(rust_type)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        SolvedType::List(item) => format!("Vec<{}>", rust_type(item)?),
        SolvedType::Option(item) => format!("Option<{}>", rust_type(item)?),
        SolvedType::Function { .. }
        | SolvedType::Record(_)
        | SolvedType::Variant(_)
        | SolvedType::Mu(_) => return Err(ProjectError::Unsupported(ty.render())),
    })
}

fn typescript_type(ty: &SolvedType) -> Result<String, ProjectError> {
    Ok(match ty {
        SolvedType::Meta(_) | SolvedType::Bound(_) => {
            return Err(ProjectError::Unresolved(ty.render()));
        }
        SolvedType::Primitive(crate::Primitive::Bool) => "boolean".into(),
        SolvedType::Primitive(crate::Primitive::Bytes) => "Uint8Array".into(),
        SolvedType::Primitive(crate::Primitive::F64 | crate::Primitive::I64) => "number".into(),
        SolvedType::Primitive(crate::Primitive::Str) => "string".into(),
        SolvedType::Primitive(crate::Primitive::Unit) => "null".into(),
        SolvedType::Tuple(items) => format!(
            "readonly [{}]",
            items
                .iter()
                .map(typescript_type)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        SolvedType::List(item) => format!("ReadonlyArray<{}>", typescript_type(item)?),
        SolvedType::Option(item) => format!("{} | null", typescript_type(item)?),
        SolvedType::Function { parameters, result } => format!(
            "({}) => {}",
            parameters
                .iter()
                .enumerate()
                .map(|(index, ty)| Ok(format!("p{index}: {}", typescript_type(ty)?)))
                .collect::<Result<Vec<_>, ProjectError>>()?
                .join(", "),
            typescript_type(result)?
        ),
        SolvedType::Record(row) => {
            ensure_closed(row)?;
            format!(
                "{{ {} }}",
                row.fields
                    .iter()
                    .map(|(label, ty)| Ok(format!("readonly {label}: {};", typescript_type(ty)?)))
                    .collect::<Result<Vec<_>, ProjectError>>()?
                    .join(" ")
            )
        }
        SolvedType::Variant(row) => {
            ensure_closed(row)?;
            row.fields
                .iter()
                .map(|(label, ty)| {
                    Ok(format!(
                        "{{ readonly tag: \"{label}\"; readonly value: {} }}",
                        typescript_type(ty)?
                    ))
                })
                .collect::<Result<Vec<_>, ProjectError>>()?
                .join(" | ")
        }
        SolvedType::Mu(_) => return Err(ProjectError::Unsupported(ty.render())),
    })
}

fn ensure_closed(row: &SolvedRow) -> Result<(), ProjectError> {
    if row.tail == SolvedTail::Closed {
        Ok(())
    } else {
        Err(ProjectError::Unresolved(format!("row tail {:?}", row.tail)))
    }
}

fn upper_camel(label: &str) -> Result<String, ProjectError> {
    if !valid_name(label) {
        return Err(ProjectError::InvalidName);
    }
    let mut output = String::new();
    for word in label.split('_') {
        let mut chars = word.chars();
        if let Some(first) = chars.next() {
            output.push(first.to_ascii_uppercase());
            output.extend(chars);
        }
    }
    Ok(output)
}
