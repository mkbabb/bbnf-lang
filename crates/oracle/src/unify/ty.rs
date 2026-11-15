//! The value types of LAYOUT-HM-V2 §1.2, the three-point kind chain, and the
//! variable supply that records each variable's kind.

/// The fifteen base types `B`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Base {
    Unit,
    Span,
    Bool,
    Str,
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

impl Base {
    pub(crate) const ALL: [Base; 15] = [
        Base::Unit,
        Base::Span,
        Base::Bool,
        Base::Str,
        Base::U8,
        Base::U16,
        Base::U32,
        Base::U64,
        Base::Usize,
        Base::I8,
        Base::I16,
        Base::I32,
        Base::I64,
        Base::F32,
        Base::F64,
    ];

    /// The least kind holding this base type: the eleven numeric members are
    /// `num`, the other four `eq`.
    pub(crate) fn kind(self) -> Kind {
        match self {
            Base::Unit | Base::Span | Base::Bool | Base::Str => Kind::Eq,
            _ => Kind::Num,
        }
    }
}

/// The kind of a type variable: `num ⊂ eq ⊂ any`. The derived order is the
/// chain's (`Num < Eq < Any`), so the meet of two kinds is their minimum.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Kind {
    Num,
    Eq,
    Any,
}

pub(crate) type Var = u32;

/// A type constructor. Arity: `Base`, `Host`, `Named` take none; `Option`,
/// `List` one; `Prod`, `Sum` at least two; `Fn` its parameters then its result.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Head {
    Base(Base),
    /// `Host(T)`: an opaque host type; unifies only with itself.
    Host(String),
    /// `R`: the nominal type of a concrete rule or instance.
    Named(String),
    Prod,
    Sum,
    Option,
    List,
    Fn,
}

/// `τ ::= α | C(τ̄)`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Ty {
    Var(Var),
    Con(Head, Vec<Ty>),
}

impl Ty {
    pub(crate) fn base(b: Base) -> Ty {
        Ty::Con(Head::Base(b), Vec::new())
    }

    pub(crate) fn list(t: Ty) -> Ty {
        Ty::Con(Head::List, vec![t])
    }

    pub(crate) fn func(params: Vec<Ty>, result: Ty) -> Ty {
        let mut args = params;
        args.push(result);
        Ty::Con(Head::Fn, args)
    }

    /// Does variable `v` occur in this type?
    pub(crate) fn mentions(&self, v: Var) -> bool {
        match self {
            Ty::Var(w) => *w == v,
            Ty::Con(_, args) => args.iter().any(|a| a.mentions(v)),
        }
    }

    /// The free variables, in order of first occurrence, without repeats.
    pub(crate) fn free_vars(&self) -> Vec<Var> {
        fn walk(t: &Ty, out: &mut Vec<Var>) {
            match t {
                Ty::Var(v) if !out.contains(v) => out.push(*v),
                Ty::Var(_) => {}
                Ty::Con(_, args) => args.iter().for_each(|a| walk(a, out)),
            }
        }
        let mut out = Vec::new();
        walk(self, &mut out);
        out
    }
}

/// The variable supply. A variable's kind is fixed when it is made and never
/// changes: unification binds the variable of the larger kind to the one of
/// the smaller, so no kind is ever narrowed in place.
#[derive(Clone, Debug, Default)]
pub(crate) struct Vars {
    kinds: Vec<Kind>,
}

impl Vars {
    pub(crate) fn fresh(&mut self, kind: Kind) -> Var {
        self.kinds.push(kind);
        (self.kinds.len() - 1) as Var
    }

    pub(crate) fn kind(&self, v: Var) -> Kind {
        self.kinds[v as usize]
    }

    /// The least kind holding `t`: a variable's own kind; a base type's kind;
    /// `any` for every other constructor (`eq` holds the base types only).
    pub(crate) fn kind_of(&self, t: &Ty) -> Kind {
        match t {
            Ty::Var(v) => self.kind(*v),
            Ty::Con(Head::Base(b), _) => b.kind(),
            Ty::Con(..) => Kind::Any,
        }
    }
}
