(** dwarfsynth
 *
 * Entry point for the BAP plugin `dwarfsynth`, defining the command line
 * interface
 **)

module Self = struct
  include Bap.Std.Self()
end

let main = DwarfSynth.Main.main

module Cmdline = struct
  module Cnf = Self.Config

  let outfile = Cnf.(
      param (string) "output"
        ~doc:("The file in which the output marshalled data will be written. "
              ^ "Output goes to ./tmp.marshal by default.")
        ~default:"tmp.marshal"
    )

  let no_rbp_undef = Cnf.(
      param (bool) "no-rbp-undef"
        ~doc:("Do not unset %rbp after it has been set once in a FDE. "
              ^"This mimics gcc eh_frame for ease of validation.")
        ~as_flag:true
        ~default:false
    )

  let timers = Cnf.(
      param (bool) "timers"
        ~doc:("Enable timers: print time probes at various points of the "
              ^"code.")
        ~as_flag:true
        ~default: false
    )

  let () = Cnf.(
      when_ready ((fun {get=(!!)} ->
          Bap.Std.Project.register_pass' (main
                                            ~no_rbp_undef:!!no_rbp_undef
                                            ~timers:!!timers
                                            !!outfile )))
    )
end
