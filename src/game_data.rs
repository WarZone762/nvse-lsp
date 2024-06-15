use std::{collections::HashMap, fmt::Display};

use la_arena::{Arena, Idx};
use serde::{Deserialize, Serialize};

use crate::{
    db::{Database, Lookup},
    hir::ty::{FunctionSignature, InferredType, Type},
};

#[derive(Debug)]
pub(crate) struct GlobalsDatabase {
    pub name: String,
    pub globals: HashMap<String, InferredType>,
}

impl GlobalsDatabase {
    pub fn new(name: String) -> Self {
        Self { name, globals: HashMap::new() }
    }

    pub fn add_global(&mut self, name: String, ty: InferredType) {
        self.globals.insert(name, ty);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct GlobalsDatabaseId(pub Idx<GlobalsDatabase>);

impl Lookup for GlobalsDatabaseId {
    type DB = Database;
    type Output = GlobalsDatabase;

    fn lookup<'a>(&self, db: &'a Self::DB) -> &'a Self::Output {
        &db.globals_dbs[self.0]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Form {
    Quest { script: Option<String> },
    Any,
    Other(FormType),
}

impl Display for Form {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Form::Quest { script, .. } => {
                if let Some(script) = script {
                    write!(f, "Quest({script})")
                } else {
                    write!(f, "Quest")
                }
            }
            Form::Any => write!(f, "Any Form"),
            Form::Other(x) => write!(f, "{x:?}"),
        }
    }
}

impl From<FormData> for Form {
    fn from(value: FormData) -> Self {
        match value.type_ {
            FormType::QUST => Self::Quest { script: value.script },
            _ => Self::Other(value.type_),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FnData {
    pub name: String,
    pub alias: Option<String>,
    pub params: Vec<FnParamData>,
    pub ret_type: TypeName,
    pub opcode: u32,
    pub is_cond_func: bool,
    pub req_ref: bool,
    pub desc: Option<String>,
}

impl From<FnData> for InferredType {
    fn from(value: FnData) -> Self {
        Self::concrete(Type::Function(Box::new(FunctionSignature {
            ret: InferredType::from(value.ret_type).narrowest,
            params: value.params.into_iter().map(|x| InferredType::from(x).narrowest).collect(),
        })))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct FnParamData {
    r#type: TypeName,
    name: Option<String>,
    optional: bool,
}

impl From<FnParamData> for InferredType {
    fn from(value: FnParamData) -> Self {
        let mut ty = Self::from(value.r#type);
        if value.optional {
            ty.widest = Type::Union(vec![ty.widest, Type::Void]);
            ty.narrowest = Type::Union(vec![ty.narrowest, Type::Void]);
        }
        ty
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum TypeName {
    AIPackage,
    Actor,
    ActorBase,
    ActorValue,
    Alignment,
    Ambiguous,
    AnimationGroup,
    AnyForm,
    AnyType,
    Array,
    #[serde(rename = "Array index")]
    ArrayIndex,
    Axis,
    Bool,
    CaravanDeck,
    Casino,
    Cell,
    Challenge,
    Class,
    CombatStyle,
    Container,
    CrimeType,
    CriticalStage,
    Double,
    EffectShader,
    EncounterZone,
    EquipType,
    Faction,
    Float,
    Form,
    FormList,
    FormType,
    Furniture,
    Global,
    IdleForm,
    ImageSpace,
    ImageSpaceModifier,
    Integer,
    InvObjectOrFormList,
    LeveledChar,
    LeveledCreature,
    LeveledItem,
    LeveledOrBaseChar,
    LeveledOrBaseCreature,
    MagicEffect,
    MagicItem,
    MapMarker,
    Message,
    MiscStat,
    #[allow(clippy::upper_case_acronyms)]
    NPC,
    NonFormList,
    Note,
    Number,
    Object,
    ObjectID,
    ObjectRef,
    #[serde(rename = "1/0")]
    OneOrZero,
    Owner,
    Pair,
    Perk,
    Quest,
    QuestStage,
    Race,
    Region,
    Reputation,
    ScriptVar,
    Sex,
    Slice,
    Sound,
    SoundFile,
    SpellItem,
    String,
    StringOrNumber,
    StringVar,
    Topic,
    #[serde(rename = "unk2E")]
    Unk2E,
    #[serde(rename = "<unknown>")]
    Unknown,
    Variable,
    VariableName,
    WeatherID,
    WorldSpace,
}

impl From<TypeName> for InferredType {
    fn from(value: TypeName) -> Self {
        match value {
            TypeName::Ambiguous => Self::any(),
            TypeName::AnyType => Self::any(),
            TypeName::Array => Self::array(),
            TypeName::ArrayIndex => Self::number(),
            TypeName::Bool => Self::bool(),
            TypeName::Double => Self::number(),
            TypeName::Float => Self::number(),
            TypeName::Integer => Self::number(),
            TypeName::MapMarker => Self::ref_(),
            TypeName::Number => Self::number(),
            TypeName::Object => Self::ref_(),
            TypeName::ObjectID => Self::ref_(),
            TypeName::ObjectRef => Self::ref_(),
            TypeName::OneOrZero => Self::bool(),
            TypeName::Owner => Self::ref_(),
            TypeName::QuestStage => Self::number(),
            TypeName::ScriptVar => Self::any(),
            TypeName::String => Self::string(),
            TypeName::StringOrNumber => {
                Self::concrete(Type::Union(vec![Type::String, Type::Number]))
            }
            TypeName::StringVar => Self::string(),
            TypeName::Unk2E => Self::any(),
            TypeName::Unknown => Self::any(),
            TypeName::Variable => Self::any(),
            TypeName::VariableName => Self::any(),

            // TODO
            TypeName::Slice => Self::any(),
            TypeName::Pair => Self::any(),

            // TODO: enums
            TypeName::ActorValue => Self::any(),
            TypeName::Alignment => Self::any(),
            TypeName::AnimationGroup => Self::any(),
            TypeName::Axis => Self::any(),
            TypeName::CrimeType => Self::any(),
            TypeName::CriticalStage => Self::any(),
            TypeName::EquipType => Self::any(),
            TypeName::FormType => Self::any(),
            TypeName::MiscStat => Self::any(),
            TypeName::Sex => Self::any(),

            TypeName::AIPackage => Self::concrete(Type::Form(Form::Other(FormType::PACK))),
            TypeName::Actor => Self::concrete(Type::Form(Form::Other(FormType::ACHR))),
            TypeName::ActorBase => Self::concrete(Type::Form(Form::Other(FormType::ACHR))),
            TypeName::AnyForm => Self::concrete(Type::Form(Form::Any)),
            TypeName::CaravanDeck => Self::concrete(Type::Form(Form::Other(FormType::CDCK))),
            TypeName::Casino => Self::concrete(Type::Form(Form::Other(FormType::CSNO))),
            TypeName::Cell => Self::concrete(Type::Form(Form::Other(FormType::CELL))),
            TypeName::Challenge => Self::concrete(Type::Form(Form::Other(FormType::CHAL))),
            TypeName::Class => Self::concrete(Type::Form(Form::Other(FormType::CLAS))),
            TypeName::CombatStyle => Self::concrete(Type::Form(Form::Other(FormType::CSTY))),
            TypeName::Container => Self::concrete(Type::Form(Form::Other(FormType::CONT))),
            TypeName::EffectShader => Self::concrete(Type::Form(Form::Other(FormType::EFSH))),
            TypeName::EncounterZone => Self::concrete(Type::Form(Form::Other(FormType::ECZN))),
            TypeName::Faction => Self::concrete(Type::Form(Form::Other(FormType::FACT))),
            TypeName::Form => Self::concrete(Type::Form(Form::Any)),
            TypeName::FormList => Self::concrete(Type::Form(Form::Other(FormType::FLST))),
            TypeName::Furniture => Self::concrete(Type::Form(Form::Other(FormType::FURN))),
            TypeName::Global => Self::concrete(Type::Form(Form::Other(FormType::GLOB))),
            TypeName::IdleForm => Self::concrete(Type::Form(Form::Other(FormType::IDLE))),
            TypeName::ImageSpace => Self::concrete(Type::Form(Form::Other(FormType::IMGS))),
            TypeName::ImageSpaceModifier => Self::concrete(Type::Form(Form::Other(FormType::IMAD))),
            TypeName::InvObjectOrFormList => Self::concrete(Type::Union(vec![
                Type::Form(Form::Other(FormType::ALCH)),
                Type::Form(Form::Other(FormType::AMMO)),
                Type::Form(Form::Other(FormType::ARMO)),
                Type::Form(Form::Other(FormType::BOOK)),
                Type::Form(Form::Other(FormType::CCRD)),
                Type::Form(Form::Other(FormType::CHIP)),
                Type::Form(Form::Other(FormType::CMNY)),
                Type::Form(Form::Other(FormType::FLST)),
                Type::Form(Form::Other(FormType::IMOD)),
                Type::Form(Form::Other(FormType::KEYM)),
                Type::Form(Form::Other(FormType::MISC)),
                Type::Form(Form::Other(FormType::WEAP)),
            ])),
            TypeName::LeveledChar => Self::concrete(Type::Form(Form::Other(FormType::LVLN))),
            TypeName::LeveledCreature => Self::concrete(Type::Form(Form::Other(FormType::LVLC))),
            TypeName::LeveledItem => Self::concrete(Type::Form(Form::Other(FormType::LVLI))),
            TypeName::LeveledOrBaseChar => Self::concrete(Type::Union(vec![
                Type::Form(Form::Other(FormType::ACHR)),
                Type::Form(Form::Other(FormType::LVLN)),
            ])),
            TypeName::LeveledOrBaseCreature => Self::concrete(Type::Union(vec![
                Type::Form(Form::Other(FormType::ACRE)),
                Type::Form(Form::Other(FormType::LVLC)),
            ])),
            TypeName::MagicEffect => Self::concrete(Type::Form(Form::Other(FormType::MGEF))),
            TypeName::MagicItem => Self::concrete(Type::Form(Form::Other(FormType::ENCH))),
            TypeName::Message => Self::concrete(Type::Form(Form::Other(FormType::MESG))),
            TypeName::NPC => Self::concrete(Type::Form(Form::Other(FormType::NPC_))),
            TypeName::NonFormList => Self::concrete(Type::Form(Form::Any)),
            TypeName::Note => Self::concrete(Type::Form(Form::Other(FormType::NOTE))),
            TypeName::Perk => Self::concrete(Type::Form(Form::Other(FormType::PERK))),
            TypeName::Quest => Self::concrete(Type::Form(Form::Quest { script: None })),
            TypeName::Race => Self::concrete(Type::Form(Form::Other(FormType::RACE))),
            TypeName::Region => Self::concrete(Type::Form(Form::Other(FormType::REGN))),
            TypeName::Reputation => Self::concrete(Type::Form(Form::Other(FormType::REPU))),
            TypeName::Sound => Self::concrete(Type::Form(Form::Other(FormType::SOUN))),
            TypeName::SoundFile => Self::concrete(Type::Form(Form::Other(FormType::MUSC))),
            TypeName::SpellItem => Self::concrete(Type::Form(Form::Other(FormType::SPEL))),
            TypeName::Topic => Self::concrete(Type::Form(Form::Other(FormType::DIAL))),
            TypeName::WeatherID => Self::concrete(Type::Form(Form::Other(FormType::WTHR))),
            TypeName::WorldSpace => Self::concrete(Type::Form(Form::Other(FormType::WRLD))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct FormData {
    #[serde(rename = "type")]
    pub type_: FormType,
    pub edid: String,
    pub script: Option<String>,
}

#[allow(clippy::upper_case_acronyms, dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) enum FormType {
    ACHR,
    ACRE,
    ACTI,
    ADDN,
    ALCH,
    ALOC,
    AMEF,
    AMMO,
    ANIO,
    ARMA,
    ARMO,
    ASPC,
    AVIF,
    BOOK,
    BPTD,
    CAMS,
    CCRD,
    CDCK,
    CELL,
    CHAL,
    CHIP,
    CLAS,
    CLMT,
    CMNY,
    CONT,
    CPTH,
    CREA,
    CSNO,
    CSTY,
    DEBR,
    DEHY,
    DIAL,
    DOBJ,
    DOOR,
    ECZN,
    EFSH,
    ENCH,
    EXPL,
    EYES,
    FACT,
    FLST,
    FURN,
    GLOB,
    GMST,
    GRAS,
    HAIR,
    HDPT,
    HUNG,
    IDLE,
    IDLM,
    IMAD,
    IMGS,
    IMOD,
    INGR,
    IPCT,
    IPDS,
    KEYM,
    LGTM,
    LIGH,
    LSCR,
    LSCT,
    LTEX,
    LVLC,
    LVLI,
    LVLN,
    MESG,
    MGEF,
    MICN,
    MISC,
    MSET,
    MSTT,
    MUSC,
    NAVM,
    NOTE,
    NPC_,
    PACK,
    PERK,
    PGRE,
    PLYR,
    PROJ,
    PWAT,
    QUST,
    RACE,
    RADS,
    RCCT,
    RCPE,
    REFR,
    REGN,
    REPU,
    RGDL,
    SCOL,
    SCPT,
    SLPD,
    SOUN,
    SPEL,
    STAT,
    TACT,
    TERM,
    TREE,
    TXST,
    VTYP,
    WATR,
    WEAP,
    WRLD,
    WTHR,
}
