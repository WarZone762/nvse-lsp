use std::{collections::HashMap, fmt::Display};

use la_arena::Idx;
use serde::{Deserialize, Serialize};

use crate::{
    db::{Database, Lookup},
    hir::ty::{Function, Type},
};

#[derive(Debug, Clone)]
pub(crate) struct GlobalsDatabase {
    pub name: String,
    pub globals: HashMap<String, Type>,
}

impl GlobalsDatabase {
    pub fn new(name: String) -> Self {
        Self { name, globals: HashMap::new() }
    }

    pub fn add_global(&mut self, name: String, ty: Type) {
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

impl From<FnData> for Type {
    fn from(value: FnData) -> Self {
        Type::Function(Box::new(Function {
            generic_args: vec![],
            ret: Type::from(value.ret_type),
            params: value.params.into_iter().map(Type::from).collect(),
        }))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct FnParamData {
    r#type: TypeName,
    name: Option<String>,
    optional: bool,
}

impl From<FnParamData> for Type {
    fn from(value: FnParamData) -> Self {
        let mut ty = Self::from(value.r#type);
        if value.optional {
            ty = Type::Union(vec![ty, Type::Void]);
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

impl From<TypeName> for Type {
    fn from(value: TypeName) -> Self {
        match value {
            TypeName::Ambiguous => Self::Any,
            TypeName::AnyType => Self::Any,
            TypeName::Array => Self::Map(Box::new((Self::Number, Self::Any))),
            TypeName::ArrayIndex => Self::Number,
            TypeName::Bool => Self::Bool,
            TypeName::Double => Self::Number,
            TypeName::Float => Self::Number,
            TypeName::Integer => Self::Number,
            TypeName::MapMarker => Self::Ref,
            TypeName::Number => Self::Number,
            TypeName::Object => Self::Ref,
            TypeName::ObjectID => Self::Ref,
            TypeName::ObjectRef => Self::Ref,
            TypeName::OneOrZero => Self::Bool,
            TypeName::Owner => Self::Ref,
            TypeName::QuestStage => Self::Number,
            TypeName::ScriptVar => Self::Any,
            TypeName::String => Self::String,
            TypeName::StringOrNumber => Self::Union(vec![Type::String, Type::Number]),
            TypeName::StringVar => Self::String,
            TypeName::Unk2E => Self::Any,
            TypeName::Unknown => Self::Any,
            TypeName::Variable => Self::Any,
            TypeName::VariableName => Self::Any,

            // TODO
            TypeName::Slice => Self::Any,
            TypeName::Pair => Self::Any,

            // TODO: enums
            TypeName::ActorValue => Self::Any,
            TypeName::Alignment => Self::Any,
            TypeName::AnimationGroup => Self::Any,
            TypeName::Axis => Self::Any,
            TypeName::CrimeType => Self::Any,
            TypeName::CriticalStage => Self::Any,
            TypeName::EquipType => Self::Any,
            TypeName::FormType => Self::Any,
            TypeName::MiscStat => Self::Any,
            TypeName::Sex => Self::Any,

            TypeName::AIPackage => Self::Form(Form::Other(FormType::PACK)),
            TypeName::Actor => Self::Form(Form::Other(FormType::ACHR)),
            TypeName::ActorBase => Self::Form(Form::Other(FormType::ACHR)),
            TypeName::AnyForm => Self::Form(Form::Any),
            TypeName::CaravanDeck => Self::Form(Form::Other(FormType::CDCK)),
            TypeName::Casino => Self::Form(Form::Other(FormType::CSNO)),
            TypeName::Cell => Self::Form(Form::Other(FormType::CELL)),
            TypeName::Challenge => Self::Form(Form::Other(FormType::CHAL)),
            TypeName::Class => Self::Form(Form::Other(FormType::CLAS)),
            TypeName::CombatStyle => Self::Form(Form::Other(FormType::CSTY)),
            TypeName::Container => Self::Form(Form::Other(FormType::CONT)),
            TypeName::EffectShader => Self::Form(Form::Other(FormType::EFSH)),
            TypeName::EncounterZone => Self::Form(Form::Other(FormType::ECZN)),
            TypeName::Faction => Self::Form(Form::Other(FormType::FACT)),
            TypeName::Form => Self::Form(Form::Any),
            TypeName::FormList => Self::Form(Form::Other(FormType::FLST)),
            TypeName::Furniture => Self::Form(Form::Other(FormType::FURN)),
            TypeName::Global => Self::Form(Form::Other(FormType::GLOB)),
            TypeName::IdleForm => Self::Form(Form::Other(FormType::IDLE)),
            TypeName::ImageSpace => Self::Form(Form::Other(FormType::IMGS)),
            TypeName::ImageSpaceModifier => Self::Form(Form::Other(FormType::IMAD)),
            TypeName::InvObjectOrFormList => Type::Union(vec![
                Self::Form(Form::Other(FormType::ALCH)),
                Self::Form(Form::Other(FormType::AMMO)),
                Self::Form(Form::Other(FormType::ARMO)),
                Self::Form(Form::Other(FormType::BOOK)),
                Self::Form(Form::Other(FormType::CCRD)),
                Self::Form(Form::Other(FormType::CHIP)),
                Self::Form(Form::Other(FormType::CMNY)),
                Self::Form(Form::Other(FormType::FLST)),
                Self::Form(Form::Other(FormType::IMOD)),
                Self::Form(Form::Other(FormType::KEYM)),
                Self::Form(Form::Other(FormType::MISC)),
                Self::Form(Form::Other(FormType::WEAP)),
            ]),
            TypeName::LeveledChar => Self::Form(Form::Other(FormType::LVLN)),
            TypeName::LeveledCreature => Self::Form(Form::Other(FormType::LVLC)),
            TypeName::LeveledItem => Self::Form(Form::Other(FormType::LVLI)),
            TypeName::LeveledOrBaseChar => Type::Union(vec![
                Self::Form(Form::Other(FormType::ACHR)),
                Self::Form(Form::Other(FormType::LVLN)),
            ]),
            TypeName::LeveledOrBaseCreature => Type::Union(vec![
                Self::Form(Form::Other(FormType::ACRE)),
                Self::Form(Form::Other(FormType::LVLC)),
            ]),
            TypeName::MagicEffect => Self::Form(Form::Other(FormType::MGEF)),
            TypeName::MagicItem => Self::Form(Form::Other(FormType::ENCH)),
            TypeName::Message => Self::Form(Form::Other(FormType::MESG)),
            TypeName::NPC => Self::Form(Form::Other(FormType::NPC_)),
            TypeName::NonFormList => Self::Form(Form::Any),
            TypeName::Note => Self::Form(Form::Other(FormType::NOTE)),
            TypeName::Perk => Self::Form(Form::Other(FormType::PERK)),
            TypeName::Quest => Self::Form(Form::Quest { script: None }),
            TypeName::Race => Self::Form(Form::Other(FormType::RACE)),
            TypeName::Region => Self::Form(Form::Other(FormType::REGN)),
            TypeName::Reputation => Self::Form(Form::Other(FormType::REPU)),
            TypeName::Sound => Self::Form(Form::Other(FormType::SOUN)),
            TypeName::SoundFile => Self::Form(Form::Other(FormType::MUSC)),
            TypeName::SpellItem => Self::Form(Form::Other(FormType::SPEL)),
            TypeName::Topic => Self::Form(Form::Other(FormType::DIAL)),
            TypeName::WeatherID => Self::Form(Form::Other(FormType::WTHR)),
            TypeName::WorldSpace => Self::Form(Form::Other(FormType::WRLD)),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn a() {
        let data = include_str!("../resources/forms/FalloutNV.esm.json");

        let parsed = serde_json::from_str::<Vec<FormData>>(data);
        println!("{parsed:#?}");
    }

    #[test]
    fn b() {
        macro_rules! print_data {
            ($name:literal) => {
                let data = include_str!(concat!("../resources/functions/", $name, ".json"));

                match serde_json::from_str::<Vec<FnData>>(data) {
                    Ok(_) => (),
                    Err(x) => println!("{x}"),
                }
                // let parsed = serde_json::from_str::<Vec<FnData>>(data).unwrap();
                // println!("{parsed:#?}");
            };
        }

        print_data!("Base game");
        print_data!("JIP LN NVSE");
        print_data!("JohnnyGuitarNVSE");
        print_data!("kNVSE");
        print_data!("lStewieAl's Tweaks");
        print_data!("MCM Extensions");
        print_data!("NVSE");
        print_data!("ShowOffNVSE Plugin");
        print_data!("ttw_nvse");

        // let data = include_str!("../resources/functions/NVSE.json");
        //
        // let parsed = serde_json::from_str::<Vec<FnData>>(data);
        // println!("{parsed:#?}");
    }
}
