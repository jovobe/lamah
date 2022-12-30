use std::io::Write;

enum Instr {
    Nop,
    LdBcD16(u16),
    LdBcA,
}

impl Instr {
    fn encode(&self, out: &mut Vec<u8>) {
        let out = match self {
            Self::Nop => out.write_all(&[0x00]),
            Self::LdBcD16(d16) => {
                out.write_all(&[0x01]);
                out.write_all(&d16.to_le_bytes());
            }
            // ...
        }
    }
}

macro_rules! gen_instrs {
  // ...
};

gen_instr! [
    (0x00, "NOP", 0, ),
    (0x01, "BCF", 2, )
];

fn gen_add_eq() -> Vec<Instr> {
    let foo = 13;

    vec![
        instr!(add (BC), A),
        instr!(ld (BC), foo),
        instr!(ld (DE), foo),
    ]
}
