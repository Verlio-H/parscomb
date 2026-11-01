module sml_parser_mod
   use parscomb_mod
   use parscomb_maps_mod, only: string_reduction, arrayify, box_array, atoi, htoi, itoc, atof
   use common_parsers_mod, only: alpha_parser, dec_parser, dec_digit_parser, hex_parser, hex_digit_parser, &
      any_parser, whitespace_parser, escape_seq_parser, eof_parser, whitespace_char_parser
   implicit none (type, external)

   ! Full parser
   type(right_parser), target       :: sml_parser
   type(left_parser), target        :: sml_topdec_parser

   ! Common helpers
   type(left_parser), target        :: sml_comment_parser
   type(or_parser), target          :: sml_inside_comment_parser
   type(or_parser), target          :: sym_sep
   type(or_parser), target          :: token_sep
   type(repitition_parser), target  :: opt_ws
   type(or_parser), target          :: ws
   type(or_parser), target          :: tok_ws
   type(right_parser), target       :: str_valid_chars_parser
   type(right_parser), target       :: str_break_parser
   type(or_parser), target          :: id_char_parser
   type(non_empty_parser), target   :: sml_escape_parser
   type(non_empty_parser), target   :: sml_id_parser
   type(non_empty_parser), target   :: sml_symid_parser
   type(map_parser), target         :: sml_var_parser
   type(or_parser), target          :: sml_opid_parser
   type(or_parser), target          :: sml_longopid_parser

   ! Constants
   type(left_parser), target        :: sml_int_parser
   type(left_parser), target        :: sml_word_parser
   type(left_parser), target        :: sml_float_parser
   type(non_empty_parser), target   :: sml_ascii_parser
   type(left_parser), target        :: sml_char_parser
   type(left_parser), target        :: sml_string_parser
   type(error_parser), target       :: sml_scon_parser
   type(error_parser), target       :: sml_con_parser

   ! Identifiers
   type(tag_parser), target         :: sml_vid_parser
   type(tag_parser), target         :: sml_longvid_parser
   type(tag_parser), target         :: sml_tyvar_parser
   type(right_parser), target       :: sml_tycon_parser
   type(tag_parser), target         :: sml_longtycon_parser
   type(left_parser), target        :: sml_lab_parser
   type(tag_parser), target         :: sml_strid_parser
   type(tag_parser), target         :: sml_longstrid_parser

   ! Expression types
   type(tag_parser), target         :: sml_pat_parser
   type(or_parser), target          :: sml_ty_parser
   type(error_parser), target          :: sml_exp_parser

   ! Pattern expressions
   type(or_parser), target          :: sml_patrow_parser
   type(chain_parser), target       :: sml_patseq_parser
   type(or_parser), target          :: sml_atpat_parser
   type(non_empty_parser), target   :: sml_conpat_parser

   ! Type expressions
   type(chain_parser), target       :: sml_tyrow_parser
   type(or_parser), target          :: sml_atty_parser
   type(or_parser), target          :: sml_tyarg_parser
   type(or_parser), target          :: sml_tyapp_parser
   type(or_parser), target          :: sml_tuplety_parser

   ! Val expressions
   type(tag_parser), target         :: sml_match_parser
   type(chain_parser), target       :: sml_expseq_parser
   type(chain_parser), target       :: sml_explst_parser
   type(chain_parser), target       :: sml_exprow_parser
   type(error_parser), target          :: sml_atexp_parser
   type(or_parser), target          :: sml_appexp_parser
   type(or_parser), target          :: sml_tyexp_parser
   type(lassoc_parser), target      :: sml_andexp_parser
   type(lassoc_parser), target      :: sml_orexp_parser
   type(lassoc_parser), target      :: sml_handleexp_parser
   type(tag_parser), target         :: sml_fnexp_parser

   ! Declarations
   type(repitition_parser), target  :: sml_decs_parser
   type(or_parser), target          :: sml_dec_parser
   type(or_parser), target          :: sml_tyvarseq_parser
   type(or_parser), target          :: sml_valbind_parser
   type(chain_parser), target       :: sml_fvalrule_parser
   type(chain_parser), target       :: sml_fvalbind_parser
   type(chain_parser), target       :: sml_typbind_parser
   type(chain_parser), target       :: sml_datbind_parser
   type(chain_parser), target       :: sml_conbind_parser
   type(chain_parser), target       :: sml_exbind_parser
   type(tag_parser), target         :: sml_withtyp_parser

   ! Signatures
   type(chain_parser), target       :: sml_condesc_parser
   type(chain_parser), target       :: sml_datdesc_parser
   type(or_parser), target          :: sml_spec_parser
   type(or_parser), target          :: sml_sharing_spec_parser
   type(repitition_parser), target  :: sml_specs_parser
   type(chain_parser), target       :: sml_sigdec_parser
   type(tag_parser), target         :: sml_sigbind_parser
   type(chain_parser), target       :: sml_sigexp_parser
   type(or_parser), target          :: sml_sigexp_sig_parser
   type(tag_parser), target         :: sml_wheretype_parser

   ! Structures
   type(or_parser), target          :: sml_sigconstraint_parser
   type(or_parser), target          :: sml_functorarg_parser
   type(error_parser), target       :: sml_atstrexp_parser
   type(lassoc_parser), target      :: sml_strexp_parser
   type(or_parser), target          :: sml_strdec_parser
   type(repitition_parser), target  :: sml_strdecs_parser
   type(tag_parser), target         :: sml_strbind_parser
   type(tag_parser), target         :: sml_functorparam_parser
   type(tag_parser), target         :: sml_funbind_parser
   type(chain_parser), target       :: sml_fundec_parser

contains
   subroutine sml_build()
      ! Helpers
      sml_inside_comment_parser = &
         (.no.str_parser('(*') > .no.str_parser('*)')) > any_parser() .or. &
         .ref.sml_comment_parser
      sml_comment_parser = &
         (str_parser('(*') > .rep.sml_inside_comment_parser) < str_parser('*)')
      token_sep = whitespace_char_parser() .or. option_parser('!%&$#+-/:<=>?@\~`^|*''"()[]{};=,')
      opt_ws = .rep.(whitespace_char_parser() .or. sml_comment_parser)
      ws = (.has.token_sep > opt_ws) .or. eof_parser()
      sym_sep = whitespace_char_parser() .or. alpha_parser() .or. option_parser('0123456789()[]{};,')
      tok_ws = (.has.sym_sep > opt_ws) .or. eof_parser()

      ! Constants
      sml_int_parser = (map_parser( &
         .opt.char_parser('~') ** (str_parser('0x') > hex_parser() .or. dec_parser()), &
         int_map &
      ) .error. 'Invalid int' .tag. 'int') < ws
      sml_word_parser = (( &
         str_parser('0w') > (dec_parser() .or. char_parser('x') > hex_parser()) &
      ) .error. 'Invalid word' .tag. 'word') < ws
      sml_float_parser = (map_parser( &
         map_parser( &
            .opt.char_parser('~') ** .oneplus.dec_digit_parser() ** ( &
               .opt.(char_parser('.') ** .oneplus.dec_digit_parser()) ** &
               (char_parser('e') ** .opt.char_parser('~') ** .oneplus.dec_digit_parser()) &
               .or. char_parser('.') ** .oneplus.dec_digit_parser() &
            ), &
            string_reduction &
         ), &
         atof &
      ) .ifempty. 'Invalid float' .tag. 'float') < ws
      str_valid_chars_parser = (.no.char_parser('\') > .no.char_parser(achar(10))) > any_parser()
      sml_escape_parser = escape_seq_parser() .or. char_parser('\') > map_parser( &
         map_parser( &
            3 * dec_digit_parser(), &
            atoi &
         ) .or. &
         char_parser('u') > map_parser( &
            4 * hex_digit_parser(), &
            htoi &
         ) .or. &
         char_parser('^') > (map_parser( &
            any_parser(), &
            map_control_char &
         )) .ifempty. 'Invalid number in escape sequence', &
         itoc &
      ) .ifempty. 'Invalid escape sequence'
      str_break_parser = &
         ((char_parser('\') > whitespace_parser()) > char_parser('\')) > succeed_parser()
      sml_ascii_parser = &
         .no.char_parser('"') > (str_valid_chars_parser .or. sml_escape_parser) .ifempty. 'Invalid char'
      sml_char_parser = (( &
         (str_parser('#"') > sml_ascii_parser) < char_parser('"') &
      ) .tag. 'char') < ws
      sml_string_parser = ( &
         map_parser( &
            (char_parser('"') > .rep.(sml_ascii_parser .or. str_break_parser)) < char_parser('"'), &
            string_reduction &
         ) .tag. 'string' &
      ) < ws

      sml_scon_parser = &
         sml_word_parser .or. &
         sml_int_parser .or. &
         sml_char_parser .or. &
         sml_string_parser .annotate. 'Invalid constant: '

      sml_con_parser = &
         sml_scon_parser .or. &
         sml_float_parser .annotate. 'Invalid constant: '

      ! Identifiers
      id_char_parser = alpha_parser() .or. dec_digit_parser() .or. char_parser('_') .or. char_parser('''')
      sml_id_parser = map_parser( &
         map_parser( &
            alpha_parser() ** .rep.id_char_parser, &
            string_reduction &
         ), &
         id_map &
      ) .ifempty. 'Identifier must not be reserved word'
      sml_symid_parser = map_parser( &
         map_parser( &
            .oneplus.predicate_parser(is_sml_symbol, 'Invalid character in identifier'), &
            string_reduction &
         ), &
         sym_map &
      ) .ifempty. 'Identifier must not be reserved word'
      sml_var_parser = map_parser( &
         char_parser('''') ** .rep.id_char_parser, &
         string_reduction &
      )
      sml_vid_parser = (sml_id_parser < ws .or. sml_symid_parser < tok_ws) .tag. 'id'
      sml_longvid_parser = .rep.(sml_id_parser < char_parser('.')) ** (sml_id_parser < ws .or. sml_symid_parser < tok_ws) .tag. 'id'
      sml_tyvar_parser = sml_var_parser < ws .tag. 'id'
      sml_tycon_parser = .no.s('*') > sml_vid_parser
      sml_longtycon_parser = .rep.(sml_id_parser < char_parser('.')) ** &
         (sml_id_parser < ws .or. (.no.s('*') > sml_symid_parser) < tok_ws) .tag. 'id'
      sml_lab_parser = (sml_id_parser .or. .no.char_parser('0') > dec_parser()) < ws
      sml_strid_parser = sml_vid_parser
      sml_longstrid_parser = sml_longvid_parser

      sml_opid_parser = &
         (w('op') > (sml_vid_parser .or. (s('=') .tag. 'id')) .tag. 'op') .or. &
         sml_vid_parser
      sml_longopid_parser = &
         (w('op') > (sml_longvid_parser .or. (s('=') .tag. 'id')) .tag. 'op') .or. &
         sml_longvid_parser

      ! Pattern expressions
      sml_patrow_parser = &
         s('...') .or. ( &
            (sml_lab_parser ** s('=') > .ref.sml_pat_parser .tag. 'patrow') .or. ( &
               sml_vid_parser ** &
               .opt.(s(':') > .ref.sml_ty_parser) ** &
               (w('as') > .ref.sml_pat_parser) .tag. 'labelvar') &
         ) ** &
         .opt.(t(',') > .ref.sml_patrow_parser)
      sml_patseq_parser = &
         .ref.sml_pat_parser .sepby. t(',')
      sml_atpat_parser = &
         (sml_opid_parser ** .opt.(s(':') > .ref.sml_ty_parser) ** &
            (w('as') ** .ref.sml_pat_parser) .tag. 'layered') .or. &
         char_parser('_') < ws .or. &
         sml_scon_parser .or. &
         sml_longopid_parser .or. &
         ((t('{') > sml_patrow_parser) < t('}') .tag. 'record') .or. &
         ((t('(') > sml_patseq_parser) < t(')') .tag. 'tuple') .or. &
         ((t('[') > .opt.sml_patseq_parser) < t(']') .tag. 'list')
      sml_conpat_parser = &
         .oneplus.sml_atpat_parser
      sml_pat_parser = &
         ((sml_conpat_parser .annotate. 'Invalid pattern') ** .rep.(s(':') > .ref.sml_ty_parser) .tag. 'pat')

      ! Types
      sml_tyrow_parser = &
         (sml_lab_parser < s(':')) ** .ref.sml_ty_parser .sepby. t(',')
      sml_atty_parser = &
         sml_tyvar_parser .or. &
         sml_longtycon_parser .or. &
         (t('(') > .ref.sml_ty_parser) < t(')') .or. &
         ((t('{') > .opt.sml_tyrow_parser) < t('}') .tag. 'record-type')
      sml_tyarg_parser = &
         sml_atty_parser .or. &
         ((t('(') > (sml_ty_parser .sepby. t(','))) < t(')') .tag. 'type-tuple')
      sml_tyapp_parser = &
         (.opt.sml_tyarg_parser ** .oneplus.sml_longtycon_parser .tag. 'type-app') .or. &
         sml_atty_parser .or. sml_longtycon_parser
      ! these could be improved
      sml_tuplety_parser = &
         (sml_tyapp_parser ** .oneplus.(s('*') > sml_tyapp_parser) .tag. 'prod-type') .or. &
         sml_tyapp_parser
      sml_ty_parser = &
         (sml_tuplety_parser ** (s('->') > .ref.sml_ty_parser) .tag. 'fn-type') .or. &
         sml_tuplety_parser
       
      ! Val expressions
      sml_match_parser = &
         .ref.sml_pat_parser < s('=>') ** &
         .ref.sml_exp_parser .sepby. s('|') .tag. 'match'
      sml_expseq_parser = &
         .ref.sml_exp_parser .sepby. t(',')
      sml_explst_parser = &
         .ref.sml_exp_parser .sepby. t(';')
      sml_exprow_parser = &
         (.ref.sml_lab_parser < s('=') .tag. 'label') ** .ref.sml_exp_parser ** &
            .ref.sml_exprow_parser .sepby. (char_parser(',') > opt_ws)
      sml_atexp_parser = sml_con_parser .or. &
         sml_longopid_parser .or. &
         (s('=') .tag. 'id') .or. &
         (char_parser('#') > sml_lab_parser .tag. 'record-selector') .or. &
         ((t('{') > .opt.sml_exprow_parser) < t('}') .tag. 'record') .or. &
         ((t('(') > .opt.sml_expseq_parser) < t(')') .tag. 'tuple') .or. &
         ((t('[') > .opt.sml_expseq_parser) < t(']') .tag. 'list') .or. &
         ((t('(') > .opt.sml_explst_parser) < t(')') .tag. 'sequence') .or. &
         ((w('let') > .ref.sml_decs_parser .tag. 'decs') ** &
            (w('in') > sml_explst_parser .tag. 'exps') < w('end') .tag. 'let') .error. 'Invalid expression'
      sml_appexp_parser = &
         (sml_atexp_parser ** .oneplus.(sml_atexp_parser) .tag. 'appexp') .or. &
         sml_atexp_parser
      sml_tyexp_parser = &
         (sml_appexp_parser ** .oneplus.(s(':') > .ref.sml_ty_parser) .tag. 'type-constraint') .or. &
         sml_appexp_parser
      sml_andexp_parser = &
         sml_tyexp_parser .sepby. w('andalso') .lassoc. 'andalso'
      sml_orexp_parser = &
         sml_andexp_parser .sepby. w('orelse') .lassoc. 'orelse'
      sml_handleexp_parser = &
         sml_orexp_parser .sepby. w('handle') .lassoc. 'handle'
      sml_fnexp_parser = &
         w('fn') > sml_match_parser .tag. 'fn'
      sml_exp_parser = &
         .ref.sml_handleexp_parser .or. &
         .ref.sml_fnexp_parser .or. &
         ( &
            (w('case') > .ref.sml_exp_parser) ** &
            (w('of') > .ref.sml_match_parser) .tag. 'case' &
         ) .or. &
         ( &
            (w('while') > .ref.sml_exp_parser) ** &
            (w('do') > .ref.sml_exp_parser) .tag. 'while' &
         ) .or. &
         ( &
            (w('if') > .ref.sml_exp_parser) ** &
            (w('then') > .ref.sml_exp_parser) ** &
            (w('else') > .ref.sml_exp_parser) .tag. 'if' &
         ) .or. &
         (w('raise') > .ref.sml_exp_parser .tag. 'raise') .error. 'Invalid expression'

      ! Declarations
      sml_tyvarseq_parser = &
         ((t('(') > sml_tyvar_parser .sepby. t(',')) < t(')') .or. &
         sml_tyvar_parser .tag. 'tyvarseq') .or. &
         succeed_parser()
      sml_valbind_parser = &
         ((sml_pat_parser < s('=')) ** .ref.sml_exp_parser .tag. 'val-bind') ** &
            .opt.(w('and') > .ref.sml_valbind_parser) .or. &
         (w('rec') ** .ref.sml_valbind_parser .tag. 'rec-bind')
      sml_fvalrule_parser = &
         ((sml_opid_parser ** .oneplus.(.ref.sml_atpat_parser) .or. &
         (t('(') > .ref.sml_atpat_parser ** sml_vid_parser ** .ref.sml_atpat_parser .tag. 'infix') ** &
            (t(')') > .rep.(.ref.sml_atpat_parser)) .or. &
         (.ref.sml_atpat_parser ** sml_vid_parser ** .ref.sml_atpat_parser .tag. 'infix')) ** &
            .opt.(s(':') > .ref.sml_ty_parser .tag. 'type-constraint') .tag. 'fun-pat') ** &
         (s('=') > .ref.sml_exp_parser)
      sml_fvalbind_parser = &
         (sml_fvalrule_parser .tag. 'fun-bind') .sepby. s('|') .sepby. w('and')
      sml_typbind_parser = &
         (sml_tyvarseq_parser ** sml_tycon_parser ** s('=') > .ref.sml_ty_parser .tag. 'type-bind') &
         .sepby. w('and')
      sml_conbind_parser = &
         sml_opid_parser ** &
            .opt.(w('of') > .ref.sml_ty_parser) .tag. 'con-bind' .sepby. s('|')
      sml_datbind_parser = &
         (sml_tyvarseq_parser ** sml_tycon_parser ** (s('=') > sml_conbind_parser) .tag. 'datatype-bind') &
         .sepby. w('and')
      sml_exbind_parser = &
         ( &
            (sml_opid_parser ** .opt.(w('of') > .ref.sml_ty_parser) .tag. 'except-con-bind') .or. &
            (sml_vid_parser ** (s('=') > sml_longvid_parser) .tag. 'except-bind') &
         ) .sepby. w('and')
      sml_withtyp_parser = &
         w('withtype') > sml_typbind_parser .tag. 'withtype'
      sml_dec_parser = &
         (w('val') > sml_tyvarseq_parser ** sml_valbind_parser) .or. &
         (w('fun') > sml_tyvarseq_parser ** sml_fvalbind_parser) .or. &
         (w('type') > sml_typbind_parser) .or. &
         (w('datatype') > sml_datbind_parser ** .opt.sml_withtyp_parser .tag. 'datatype') .or. &
         (w('datatype') > sml_tycon_parser ** &
            (((s('=') > str_parser('datatype')) > ws) > sml_longtycon_parser) .tag. 'datatype-replica') .or. &
         ((w('abstype') > sml_datbind_parser ** .opt.sml_withtyp_parser .tag. 'abstype-binding') ** &
            ((w('with') > .ref.sml_decs_parser) < w('end') .tag. 'abstype-with') .tag. 'abstype') .or. &
         (w('exception') > sml_exbind_parser) .or. &
         (w('local') > .ref.sml_decs_parser .tag. 'local-private') ** &
            ((w('in') > .ref.sml_decs_parser) < w('end') .tag. 'local-public') .or. &
         (w('open') > .rep.sml_longstrid_parser .tag. 'open') .or. &
         (w('infix') > (map_parser(dec_digit_parser(), atoi) < ws) &
            ** .oneplus.sml_vid_parser .tag. 'infix') .or. &
         (w('infixr') > (map_parser(dec_digit_parser(), atoi) < ws) &
            ** .oneplus.sml_vid_parser .tag. 'infixr') .or. &
         (w('nonfix') > .oneplus.sml_vid_parser .tag. 'nonfix')
      sml_decs_parser = &
         .rep.((.ref.sml_dec_parser .tag. 'dec') < .opt.t(';'))

      ! Signatures
      sml_condesc_parser = &
         sml_tycon_parser ** .opt.(w('of') > .ref.sml_ty_parser)
      sml_datdesc_parser = &
         sml_tyvarseq_parser ** sml_tycon_parser ** (sml_condesc_parser .tag. 'con-desc' .sepby. s('|'))
      sml_spec_parser = &
         w('val') > (((sml_vid_parser < s(':')) ** .ref.sml_ty_parser .tag. 'val-desc') .sepby. w('and')) .or. &
         w('type') > ((sml_tyvarseq_parser ** sml_tycon_parser .tag. 'type-desc') .sepby. w('and')) .or. &
         w('eqtype') > ((sml_tyvarseq_parser ** sml_tycon_parser .tag. 'eqtype-desc') .sepby. w('and')) .or. &
         w('datatype') > (sml_datdesc_parser .tag. 'datatype-desc' .sepby. w('and')) .or.  &
         w('datatype') > sml_tycon_parser ** ((s('=') > w('datatype')) > sml_tycon_parser) .or. &
         w('exception') > (sml_condesc_parser .tag. 'ex-desc' .sepby. w('and')) .or. &
         w('structure') > (sml_strid_parser ** (s(':') > .ref.sml_sigexp_parser) .tag. 'str-desc' .sepby. w('and')) .or. &
         w('include') > .ref.sml_sigexp_parser .or. &
         w('include') > (.oneplus.sml_strid_parser .tag. 'include-list')
      sml_sharing_spec_parser = &
         (w('sharing') > w('type') ** sml_longtycon_parser ** .oneplus.(s('=') > sml_longtycon_parser) &
            .tag. 'sharing-type') .or. &
         (w('sharing') > sml_longstrid_parser ** .oneplus.(s('=') > sml_longstrid_parser) .tag. 'sharing')
      sml_specs_parser = &
         .rep.(sml_spec_parser ** .rep.sml_sharing_spec_parser < .opt.t(';'))
      sml_wheretype_parser = &
         w('type') > sml_tyvarseq_parser ** sml_tycon_parser ** (s('=') > .ref.sml_ty_parser) .tag. 'wheretype'
      sml_sigexp_sig_parser = &
         ((w('sig') > .ref.sml_specs_parser) < w('end') .tag. 'sig') .or. &
         sml_strid_parser
      sml_sigexp_parser = &
         .ref.sml_sigexp_sig_parser ** .rep.sml_wheretype_parser
      sml_sigbind_parser = &
         sml_strid_parser ** (s('=') > .ref.sml_sigexp_parser) .tag. 'sig-bind'
      sml_sigdec_parser = &
         (sml_sigbind_parser .sepby. w('and'))
      
      ! Structures
      sml_sigconstraint_parser = &
         (s(':') > .ref.sml_sigexp_parser .tag. 'transparent-ascript') .or. &
         (s(':>') > .ref.sml_sigexp_parser .tag. 'opaque-ascript')
      sml_functorarg_parser = &
         .ref.sml_strdecs_parser .or. &
         .ref.sml_strexp_parser
      sml_atstrexp_parser = &
         ((w('struct') > .ref.sml_strdecs_parser) < w('end') .tag. 'struct') .or. &
         (sml_strid_parser ** (t('(') > sml_functorarg_parser) < t(')') .tag. 'functor-call') .or. &
         sml_longstrid_parser .or. &
         ((w('let') > .ref.sml_strdecs_parser .tag. 'let-strdecs') > &
            ((w('in') > .ref.sml_strexp_parser) > w('end') .tag. 'let-strexp') .tag. 'let-struct') .error. 'Invalid struct'
      sml_strexp_parser = &
         sml_atstrexp_parser ** .rep.sml_sigconstraint_parser .lassoc. 'ascribed-struct'
      sml_strbind_parser = &
         sml_strid_parser ** .opt.sml_sigconstraint_parser ** (s('=') > sml_strexp_parser) .tag. 'str-bind'
      sml_strdec_parser = &
         .ref.sml_dec_parser .or. &
         w('structure') > (sml_strbind_parser .sepby. w('and')) .or. &
         (w('local') > (.ref.sml_strdecs_parser .tag. 'local-private') ** &
            ((w('in') > (.ref.sml_strdecs_parser .tag. 'local-public')) < w('end')) .tag. 'local')
      sml_strdecs_parser = &
         .rep.(sml_strdec_parser < .opt.s(';'))
      sml_functorparam_parser = &
         (sml_strid_parser ** (s(':') > sml_sigexp_parser) .or. sml_specs_parser) .tag. 'functor-param'
      sml_funbind_parser = &
         sml_strid_parser ** ((t('(') > sml_functorparam_parser) < t(')')) ** .opt.sml_sigconstraint_parser ** &
            (s('=') > sml_strexp_parser) .tag. 'functor-bind'
      sml_fundec_parser = &
         (sml_funbind_parser .sepby. w('and'))

      ! Full parsers
      sml_topdec_parser = &
         sml_strdec_parser < .opt.t(';')
      sml_parser = opt_ws > ( &
         tern(w('signature'), sml_sigdec_parser < .opt.t(';'), &
         tern(w('functor'), sml_fundec_parser < .opt.t(';'), &
         sml_topdec_parser .or. .ref.sml_exp_parser < (t(';') .or. eof_parser()) &
      )) .until. eof_parser())

   end subroutine

   subroutine int_map(input)
      class(*), allocatable, intent(inout) :: input

      if (.not.allocated(input)) return
      select type (p => input)
      type is (pair)
         select type (i => p%b)
         type is (integer(size_t))
            if (allocated(p%a)) then
               input = -i
            else
               input = i
            end if
         end select
      end select
   end subroutine

   subroutine map_control_char(input)
      class(*), allocatable, intent(inout) :: input

      if (.not.allocated(input)) return
      select type (c => input)
      type is (character(*))
         if (c >= '@' .and. c <= '_') then
            input = int(iachar(c) - iachar('@'), size_t)
         else
            deallocate(input)
         end if
      end select
   end subroutine

   function is_sml_symbol(input) result(res)
      class(*), intent(in) :: input
      logical :: res

      res = .false.
      select type (c => input)
      type is (character(*))
         if (index('!%&$#+-/:<=>?@\~`^|*=', c) /= 0) res = .true.
      end select
   end function

   subroutine sym_map(input)
      class(*), allocatable, intent(inout) :: input

      select type (s => input)
      type is (character(*))
         ! check against reserve words
         select case (s)
         case default
         ! core reserved words
         case (':', '=', '=>', '->', '#', '|')
            deallocate(input)
         ! module reserved words
         case (':>')
            deallocate(input)
         end select
      end select
   end subroutine

   subroutine id_map(input)
      class(*), allocatable, intent(inout) :: input

      select type (s => input)
      type is (character(*))
         ! check against reserve words
         select case (s)
         case default
         ! core reserved words
         case ('abstype', 'and', 'andalso', 'as', 'case', 'datatype', 'do', 'else', 'end', 'exception', 'fn', 'fun', 'handle', &
               'if', 'in', 'infix', 'infixr', 'let', 'local', 'nonfix', 'of', 'op', 'open', 'orelse', 'raise', 'rec', 'then', 'type', &
               'val', 'with', 'withtype', 'while')
            deallocate(input)
         ! module language reserved words
         case ('eqtype', 'functor', 'include', 'sharing', 'sig', 'signature', 'struct', 'structure', 'where')
            deallocate(input)
         end select
      end select
   end subroutine

   function w(str)
      character(*), intent(in) :: str
      type(left_parser) :: w
      w = (str_parser(str) < ws)
   end function

   function s(str)
      character(*), intent(in) :: str
      type(left_parser) :: s
      s = (str_parser(str) < tok_ws)
   end function

   function t(str)
      character(*), intent(in) :: str
      type(left_parser) :: t
      t = (str_parser(str) < opt_ws)
   end function
end module
