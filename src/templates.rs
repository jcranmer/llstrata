struct Template {
    pub name: &'static str,
}

macro_rules! template {
    ($name:expr, $ins:expr, $outs:expr, $undef:expr,
     ($($subs:tt),*), $body:expr) => {
        let mut pattern = format!("  .text
  .globl {0}
  .type {0}, @function
#! maybe-read {{ {1} }}
#! maybe-write {{ {2} }}
#! must-undef {{ {3} }}
.{0}:
  {4}
  retq

.size {0}, .-{0}", $name, $ins, $outs, $undef, $body);
        let subs = vec![$($subs),*];
        for (i, sub) in subs.iter().enumerate() {
            let search = format!("{{{}}}", i);
            pattern = pattern.replace(&search, sub);
        }
        print!("{}", pattern);
    }
}

pub fn generate_templates() {
    template!("move_016_032_{0}_{1}_{2}", "%{0} %{1}", "%{2}", "%r15",
              ("r8w", "r9w", "ebx"),
 "# ----------------------------------------------------------------------------
  # moves {0} and {1} to {2}.
  # ----------------------------------------------------------------------------
  pushfq
  movw %{1}, %r15w
  shlq $0x10, %r15
  movw %{0}, %r15w
  movl %r15d, %{2}
  popfq");
}

