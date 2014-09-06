#ifndef _TERM
#define _TERM
#include <termios.h>
#ifndef _CURSES
#ifdef SMALL_CURSES
typedef unsigned short chtype ;
#else
typedef unsigned long chtype ;
#endif
#endif
#define auto_left_margin cur_term->_auto_left_margin
#define auto_right_margin cur_term->_auto_right_margin
#define back_color_erase cur_term->_back_color_erase
#define can_change cur_term->_can_change
#define ceol_standout_glitch cur_term->_ceol_standout_glitch
#define col_addr_glitch cur_term->_col_addr_glitch
#define cpi_changes_res cur_term->_cpi_changes_res
#define cr_cancels_micro_mode cur_term->_cr_cancels_micro_mode
#define dest_tabs_magic_smso cur_term->_dest_tabs_magic_smso
#define eat_newline_glitch cur_term->_eat_newline_glitch
#define erase_overstrike cur_term->_erase_overstrike
#define generic_type cur_term->_generic_type
#define hard_copy cur_term->_hard_copy
#define hard_cursor cur_term->_hard_cursor
#define has_meta_key cur_term->_has_meta_key
#define has_print_wheel cur_term->_has_print_wheel
#define has_status_line cur_term->_has_status_line
#define hue_lightness_saturation cur_term->_hue_lightness_saturation
#define insert_null_glitch cur_term->_insert_null_glitch
#define lpi_changes_res cur_term->_lpi_changes_res
#define memory_above cur_term->_memory_above
#define memory_below cur_term->_memory_below
#define move_insert_mode cur_term->_move_insert_mode
#define move_standout_mode cur_term->_move_standout_mode
#define needs_xon_xoff cur_term->_needs_xon_xoff
#define no_esc_ctlc cur_term->_no_esc_ctlc
#define non_dest_scroll_region cur_term->_non_dest_scroll_region
#define non_rev_rmcup cur_term->_non_rev_rmcup
#define no_pad_char cur_term->_no_pad_char
#define over_strike cur_term->_over_strike
#define prtr_silent cur_term->_prtr_silent
#define row_addr_glitch cur_term->_row_addr_glitch
#define semi_auto_right_margin cur_term->_semi_auto_right_margin
#define status_line_esc_ok cur_term->_status_line_esc_ok
#define tilde_glitch cur_term->_tilde_glitch
#define transparent_underline cur_term->_transparent_underline
#define xon_xoff cur_term->_xon_xoff
#define bit_image_entwining cur_term->_bit_image_entwining
#define bit_image_type cur_term->_bit_image_type
#define buffer_capacity cur_term->_buffer_capacity
#define buttons cur_term->_buttons
#define columns cur_term->_columns
#define dot_vert_spacing cur_term->_dot_vert_spacing
#define dot_horz_spacing cur_term->_dot_horz_spacing
#define init_tabs cur_term->_init_tabs
#define label_height cur_term->_label_height
#define label_width cur_term->_label_width
#define lines cur_term->_lines
#define lines_of_memory cur_term->_lines_of_memory
#define magic_cookie_glitch cur_term->_magic_cookie_glitch
#define max_colors cur_term->_max_colors
#define max_micro_address cur_term->_max_micro_address
#define max_micro_jump cur_term->_max_micro_jump
#define max_pairs cur_term->_max_pairs
#define maximum_window cur_term->_maximum_window
#define micro_col_size cur_term->_micro_col_size
#define micro_line_size cur_term->_micro_line_size
#define no_color_video cur_term->_no_color_video
#define number_of_pins cur_term->_number_of_pins
#define num_labels cur_term->_num_labels
#define output_res_char cur_term->_output_res_char
#define outpt_res_line cur_term->_outpt_res_line
#define output_res_horz_inch cur_term->_output_res_horz_inch
#define output_res_vert_inch cur_term->_output_res_vert_inch
#define padding_baud_rate cur_term->_padding_baud_rate
#define print_rate cur_term->_print_rate
#define virtual_terminal cur_term->_virtual_terminal
#define wide_char_size cur_term->_wide_char_size
#define width_status_line cur_term->_width_status_line
#define acs_chars cur_term->_acs_chars
#define alt_scancode_esc cur_term->_alt_scancode_esc
#define back_tab cur_term->_back_tab
#define bell cur_term->_bell
#define bit_image_repeat cur_term->_bit_image_repeat
#define bit_image_newline cur_term->_bit_image_newline
#define carriage_return cur_term->_carriage_return
#define change_char_pitch cur_term->_change_char_pitch
#define change_line_pitch cur_term->_change_line_pitch
#define change_res_horz cur_term->_change_res_horz
#define change_res_vert cur_term->_change_res_vert
#define change_scroll_region cur_term->_change_scroll_region
#define char_padding cur_term->_char_padding
#define char_set_names cur_term->_char_set_names
#define clear_all_tabs cur_term->_clear_all_tabs
#define clear_margins cur_term->_clear_margins
#define clear_screen cur_term->_clear_screen
#define clr_bol cur_term->_clr_bol
#define clr_eol cur_term->_clr_eol
#define clr_eos cur_term->_clr_eos
#define code_set_init cur_term->_code_set_init
#define color_names cur_term->_color_names
#define column_address cur_term->_column_address
#define command_character cur_term->_command_character
#define create_window cur_term->_create_window
#define cursor_address cur_term->_cursor_address
#define cursor_down cur_term->_cursor_down
#define cursor_home cur_term->_cursor_home
#define cursor_invisible cur_term->_cursor_invisible
#define cursor_left cur_term->_cursor_left
#define cursor_mem_address cur_term->_cursor_mem_address
#define cursor_normal cur_term->_cursor_normal
#define cursor_right cur_term->_cursor_right
#define cursor_to_ll cur_term->_cursor_to_ll
#define cursor_up cur_term->_cursor_up
#define cursor_visible cur_term->_cursor_visible
#define define_bit_image_region cur_term->_define_bit_image_region
#define define_char cur_term->_define_char
#define delete_character cur_term->_delete_character
#define delete_line cur_term->_delete_line
#define device_type cur_term->_device_type
#define dial_phone cur_term->_dial_phone
#define dis_status_line cur_term->_dis_status_line
#define display_clock cur_term->_display_clock
#define display_pc_char cur_term->_display_pc_char
#define down_half_line cur_term->_down_half_line
#define ena_acs cur_term->_ena_acs
#define end_bit_image_region cur_term->_end_bit_image_region
#define enter_alt_charset_mode cur_term->_enter_alt_charset_mode
#define enter_am_mode cur_term->_enter_am_mode
#define enter_blink_mode cur_term->_enter_blink_mode
#define enter_bold_mode cur_term->_enter_bold_mode
#define enter_ca_mode cur_term->_enter_ca_mode
#define enter_delete_mode cur_term->_enter_delete_mode
#define enter_dim_mode cur_term->_enter_dim_mode
#define enter_doublewidth_mode cur_term->_enter_doublewidth_mode
#define enter_draft_quality cur_term->_enter_draft_quality
#define enter_insert_mode cur_term->_enter_insert_mode
#define enter_italics_mode cur_term->_enter_italics_mode
#define enter_leftward_mode cur_term->_enter_leftward_mode
#define enter_micro_mode cur_term->_enter_micro_mode
#define enter_near_letter_quality cur_term->_enter_near_letter_quality
#define enter_normal_quality cur_term->_enter_normal_quality
#define enter_pc_charset_mode cur_term->_enter_pc_charset_mode
#define enter_protected_mode cur_term->_enter_protected_mode
#define enter_reverse_mode cur_term->_enter_reverse_mode
#define enter_scancode_mode cur_term->_enter_scancode_mode
#define enter_secure_mode cur_term->_enter_secure_mode
#define enter_shadow_mode cur_term->_enter_shadow_mode
#define enter_standout_mode cur_term->_enter_standout_mode
#define enter_subscript_mode cur_term->_enter_subscript_mode
#define enter_superscript_mode cur_term->_enter_superscript_mode
#define enter_underline_mode cur_term->_enter_underline_mode
#define enter_upward_mode cur_term->_enter_upward_mode
#define enter_xon_mode cur_term->_enter_xon_mode
#define erase_chars cur_term->_erase_chars
#define exit_alt_charset_mode cur_term->_exit_alt_charset_mode
#define exit_am_mode cur_term->_exit_am_mode
#define exit_attribute_mode cur_term->_exit_attribute_mode
#define exit_ca_mode cur_term->_exit_ca_mode
#define exit_delete_mode cur_term->_exit_delete_mode
#define exit_doublewidth_mode cur_term->_exit_doublewidth_mode
#define exit_insert_mode cur_term->_exit_insert_mode
#define exit_italics_mode cur_term->_exit_italics_mode
#define exit_leftward_mode cur_term->_exit_leftward_mode
#define exit_micro_mode cur_term->_exit_micro_mode
#define exit_pc_charset_mode cur_term->_exit_pc_charset_mode
#define exit_scancode_mode cur_term->_exit_scancode_mode
#define exit_shadow_mode cur_term->_exit_shadow_mode
#define exit_standout_mode cur_term->_exit_standout_mode
#define exit_subscript_mode cur_term->_exit_subscript_mode
#define exit_underline_mode cur_term->_exit_underline_mode
#define exit_upward_mode cur_term->_exit_upward_mode
#define exit_xon_mode cur_term->_exit_xon_mode
#define flash_hook cur_term->_flash_hook
#define flash_screen cur_term->_flash_screen
#define fixed_pause cur_term->_fixed_pause
#define form_feed cur_term->_form_feed
#define from_status_line cur_term->_from_status_line
#define get_mouse cur_term->_get_mouse
#define goto_window cur_term->_goto_window
#define hangup cur_term->_hangup
#define init_1string cur_term->_init_1string
#define init_2string cur_term->_init_2string
#define init_3string cur_term->_init_3string
#define init_file cur_term->_init_file
#define init_prog cur_term->_init_prog
#define initialize_color cur_term->_initialize_color
#define initialize_pair cur_term->_initialize_pair
#define insert_character cur_term->_insert_character
#define insert_line cur_term->_insert_line
#define insert_padding cur_term->_insert_padding
#define keypad_local cur_term->_keypad_local
#define keypad_xmit cur_term->_keypad_xmit
#define lab_f0 cur_term->_lab_f0
#define lab_f1 cur_term->_lab_f1
#define lab_f2 cur_term->_lab_f2
#define lab_f3 cur_term->_lab_f3
#define lab_f4 cur_term->_lab_f4
#define lab_f5 cur_term->_lab_f5
#define lab_f6 cur_term->_lab_f6
#define lab_f7 cur_term->_lab_f7
#define lab_f8 cur_term->_lab_f8
#define lab_f9 cur_term->_lab_f9
#define lab_f10 cur_term->_lab_f10
#define label_format cur_term->_label_format
#define label_off cur_term->_label_off
#define label_on cur_term->_label_on
#define meta_off cur_term->_meta_off
#define meta_on cur_term->_meta_on
#define micro_column_address cur_term->_micro_column_address
#define micro_down cur_term->_micro_down
#define micro_left cur_term->_micro_left
#define micro_right cur_term->_micro_right
#define micro_row_address cur_term->_micro_row_address
#define micro_up cur_term->_micro_up
#define mouse_info cur_term->_mouse_info
#define newline cur_term->_newline
#define order_of_pins cur_term->_order_of_pins
#define orig_colors cur_term->_orig_colors
#define orig_pair cur_term->_orig_pair
#define pad_char cur_term->_pad_char
#define parm_dch cur_term->_parm_dch
#define parm_delete_line cur_term->_parm_delete_line
#define parm_down_cursor cur_term->_parm_down_cursor
#define parm_down_micro cur_term->_parm_down_micro
#define parm_ich cur_term->_parm_ich
#define parm_index cur_term->_parm_index
#define parm_insert_line cur_term->_parm_insert_line
#define parm_left_cursor cur_term->_parm_left_cursor
#define parm_left_micro cur_term->_parm_left_micro
#define parm_right_cursor cur_term->_parm_right_cursor
#define parm_right_micro cur_term->_parm_right_micro
#define parm_rindex cur_term->_parm_rindex
#define parm_up_cursor cur_term->_parm_up_cursor
#define parm_up_micro cur_term->_parm_up_micro
#define pc_term_options cur_term->_pc_term_options
#define pkey_key cur_term->_pkey_key
#define pkey_local cur_term->_pkey_local
#define pkey_plab cur_term->_pkey_plab
#define pkey_xmit cur_term->_pkey_xmit
#define plab_norm cur_term->_plab_norm
#define print_screen cur_term->_print_screen
#define prtr_non cur_term->_prtr_non
#define prtr_off cur_term->_prtr_off
#define prtr_on cur_term->_prtr_on
#define pulse cur_term->_pulse
#define quick_dial cur_term->_quick_dial
#define remove_clock cur_term->_remove_clock
#define repeat_char cur_term->_repeat_char
#define req_for_input cur_term->_req_for_input
#define req_mouse_pos cur_term->_req_mouse_pos
#define reset_1string cur_term->_reset_1string
#define reset_2string cur_term->_reset_2string
#define reset_3string cur_term->_reset_3string
#define reset_file cur_term->_reset_file
#define restore_cursor cur_term->_restore_cursor
#define row_address cur_term->_row_address
#define save_cursor cur_term->_save_cursor
#define scancode_escape cur_term->_scancode_escape
#define scroll_forward cur_term->_scroll_forward
#define scroll_reverse cur_term->_scroll_reverse
#define select_char_set cur_term->_select_char_set
#define set0_des_seq cur_term->_set0_des_seq
#define set1_des_seq cur_term->_set1_des_seq
#define set2_des_seq cur_term->_set2_des_seq
#define set3_des_seq cur_term->_set3_des_seq
#define set_a_background cur_term->_set_a_background
#define set_a_foreground cur_term->_set_a_foreground
#define set_attributes cur_term->_set_attributes
#define set_background cur_term->_set_background
#define set_bottom_margin cur_term->_set_bottom_margin
#define set_bottom_margin_parm cur_term->_set_bottom_margin_parm
#define set_clock cur_term->_set_clock
#define set_color_band cur_term->_set_color_band
#define set_color_pair cur_term->_set_color_pair
#define set_foreground cur_term->_set_foreground
#define set_left_margin cur_term->_set_left_margin
#define set_left_margin_parm cur_term->_set_left_margin_parm
#define set_lr_margin cur_term->_set_lr_margin
#define set_page_length cur_term->_set_page_length
#define set_right_margin cur_term->_set_right_margin
#define set_right_margin_parm cur_term->_set_right_margin_parm
#define set_tab cur_term->_set_tab
#define set_tb_margin cur_term->_set_tb_margin
#define set_top_margin cur_term->_set_top_margin
#define set_top_margin_parm cur_term->_set_top_margin_parm
#define set_window cur_term->_set_window
#define start_bit_image cur_term->_start_bit_image
#define start_char_set_def cur_term->_start_char_set_def
#define stop_bit_image cur_term->_stop_bit_image
#define stop_char_set_dir cur_term->_stop_char_set_dir
#define subscript_characters cur_term->_subscript_characters
#define superscript_characters cur_term->_superscript_characters
#define tab cur_term->_tab
#define these_cause_cr cur_term->_these_cause_cr
#define to_status_line cur_term->_to_status_line
#define tone cur_term->_tone
#define underline_char cur_term->_underline_char
#define up_half_line cur_term->_up_half_line
#define user0 cur_term->_user0
#define user1 cur_term->_user1
#define user2 cur_term->_user2
#define user3 cur_term->_user3
#define user4 cur_term->_user4
#define user5 cur_term->_user5
#define user6 cur_term->_user6
#define user7 cur_term->_user7
#define user8 cur_term->_user8
#define user9 cur_term->_user9
#define wait_tone cur_term->_wait_tone
#define xoff_character cur_term->_xoff_character
#define xon_character cur_term->_xon_character
#define zero_motion cur_term->_zero_motion
#define key_break cur_term->_key_break
#define key_down cur_term->_key_down
#define key_up cur_term->_key_up
#define key_left cur_term->_key_left
#define key_right cur_term->_key_right
#define key_home cur_term->_key_home
#define key_backspace cur_term->_key_backspace
#define key_f0 cur_term->_key_f0
#define key_f1 cur_term->_key_f1
#define key_f2 cur_term->_key_f2
#define key_f3 cur_term->_key_f3
#define key_f4 cur_term->_key_f4
#define key_f5 cur_term->_key_f5
#define key_f6 cur_term->_key_f6
#define key_f7 cur_term->_key_f7
#define key_f8 cur_term->_key_f8
#define key_f9 cur_term->_key_f9
#define key_f10 cur_term->_key_f10
#define key_f11 cur_term->_key_f11
#define key_f12 cur_term->_key_f12
#define key_f13 cur_term->_key_f13
#define key_f14 cur_term->_key_f14
#define key_f15 cur_term->_key_f15
#define key_f16 cur_term->_key_f16
#define key_f17 cur_term->_key_f17
#define key_f18 cur_term->_key_f18
#define key_f19 cur_term->_key_f19
#define key_f20 cur_term->_key_f20
#define key_f21 cur_term->_key_f21
#define key_f22 cur_term->_key_f22
#define key_f23 cur_term->_key_f23
#define key_f24 cur_term->_key_f24
#define key_f25 cur_term->_key_f25
#define key_f26 cur_term->_key_f26
#define key_f27 cur_term->_key_f27
#define key_f28 cur_term->_key_f28
#define key_f29 cur_term->_key_f29
#define key_f30 cur_term->_key_f30
#define key_f31 cur_term->_key_f31
#define key_f32 cur_term->_key_f32
#define key_f33 cur_term->_key_f33
#define key_f34 cur_term->_key_f34
#define key_f35 cur_term->_key_f35
#define key_f36 cur_term->_key_f36
#define key_f37 cur_term->_key_f37
#define key_f38 cur_term->_key_f38
#define key_f39 cur_term->_key_f39
#define key_f40 cur_term->_key_f40
#define key_f41 cur_term->_key_f41
#define key_f42 cur_term->_key_f42
#define key_f43 cur_term->_key_f43
#define key_f44 cur_term->_key_f44
#define key_f45 cur_term->_key_f45
#define key_f46 cur_term->_key_f46
#define key_f47 cur_term->_key_f47
#define key_f48 cur_term->_key_f48
#define key_f49 cur_term->_key_f49
#define key_f50 cur_term->_key_f50
#define key_f51 cur_term->_key_f51
#define key_f52 cur_term->_key_f52
#define key_f53 cur_term->_key_f53
#define key_f54 cur_term->_key_f54
#define key_f55 cur_term->_key_f55
#define key_f56 cur_term->_key_f56
#define key_f57 cur_term->_key_f57
#define key_f58 cur_term->_key_f58
#define key_f59 cur_term->_key_f59
#define key_f60 cur_term->_key_f60
#define key_f61 cur_term->_key_f61
#define key_f62 cur_term->_key_f62
#define key_f63 cur_term->_key_f63
#define key_dl cur_term->_key_dl
#define key_il cur_term->_key_il
#define key_dc cur_term->_key_dc
#define key_ic cur_term->_key_ic
#define key_eic cur_term->_key_eic
#define key_clear cur_term->_key_clear
#define key_eos cur_term->_key_eos
#define key_eol cur_term->_key_eol
#define key_sf cur_term->_key_sf
#define key_sr cur_term->_key_sr
#define key_npage cur_term->_key_npage
#define key_ppage cur_term->_key_ppage
#define key_stab cur_term->_key_stab
#define key_ctab cur_term->_key_ctab
#define key_catab cur_term->_key_catab
#define key_enter cur_term->_key_enter
#define key_sreset cur_term->_key_sreset
#define key_reset cur_term->_key_reset
#define key_print cur_term->_key_print
#define key_ll cur_term->_key_ll
#define key_a1 cur_term->_key_a1
#define key_a3 cur_term->_key_a3
#define key_b2 cur_term->_key_b2
#define key_c1 cur_term->_key_c1
#define key_c3 cur_term->_key_c3
#define key_btab cur_term->_key_btab
#define key_beg cur_term->_key_beg
#define key_cancel cur_term->_key_cancel
#define key_close cur_term->_key_close
#define key_command cur_term->_key_command
#define key_copy cur_term->_key_copy
#define key_create cur_term->_key_create
#define key_end cur_term->_key_end
#define key_exit cur_term->_key_exit
#define key_find cur_term->_key_find
#define key_help cur_term->_key_help
#define key_mark cur_term->_key_mark
#define key_message cur_term->_key_message
#define key_move cur_term->_key_move
#define key_next cur_term->_key_next
#define key_open cur_term->_key_open
#define key_options cur_term->_key_options
#define key_previous cur_term->_key_previous
#define key_redo cur_term->_key_redo
#define key_reference cur_term->_key_reference
#define key_refresh cur_term->_key_refresh
#define key_replace cur_term->_key_replace
#define key_restart cur_term->_key_restart
#define key_resume cur_term->_key_resume
#define key_save cur_term->_key_save
#define key_sbeg cur_term->_key_sbeg
#define key_scancel cur_term->_key_scancel
#define key_scommand cur_term->_key_scommand
#define key_scopy cur_term->_key_scopy
#define key_screate cur_term->_key_screate
#define key_sdc cur_term->_key_sdc
#define key_sdl cur_term->_key_sdl
#define key_select cur_term->_key_select
#define key_send cur_term->_key_send
#define key_seol cur_term->_key_seol
#define key_sexit cur_term->_key_sexit
#define key_sfind cur_term->_key_sfind
#define key_shelp cur_term->_key_shelp
#define key_shome cur_term->_key_shome
#define key_sic cur_term->_key_sic
#define key_sleft cur_term->_key_sleft
#define key_smessage cur_term->_key_smessage
#define key_smove cur_term->_key_smove
#define key_snext cur_term->_key_snext
#define key_soptions cur_term->_key_soptions
#define key_sprevious cur_term->_key_sprevious
#define key_sprint cur_term->_key_sprint
#define key_sredo cur_term->_key_sredo
#define key_sreplace cur_term->_key_sreplace
#define key_sright cur_term->_key_sright
#define key_srsume cur_term->_key_srsume
#define key_ssave cur_term->_key_ssave
#define key_ssupend cur_term->_key_ssupend
#define key_sundo cur_term->_key_sundo
#define key_suspend cur_term->_key_suspend
#define key_undo cur_term->_key_undo
#define key_mouse cur_term->_key_mouse
#ifndef SMALL_CURSES
typedef struct _content {
short red ;
short green ;
short blue ;
} CONTENT ;
typedef struct _pair {
int fg ;
int bg ;
} PAIR ;
#endif
typedef struct _terminal {
char termname[15] ;
char *_termdesc ;
char _bool_starter ;
char _auto_left_margin ;
char _auto_right_margin ;
char _back_color_erase ;
char _can_change ;
char _ceol_standout_glitch ;
char _col_addr_glitch ;
char _cpi_changes_res ;
char _cr_cancels_micro_mode ;
char _dest_tabs_magic_smso ;
char _eat_newline_glitch ;
char _erase_overstrike ;
char _generic_type ;
char _hard_copy ;
char _hard_cursor ;
char _has_meta_key ;
char _has_print_wheel ;
char _has_status_line ;
char _hue_lightness_saturation ;
char _insert_null_glitch ;
char _lpi_changes_res ;
char _memory_above ;
char _memory_below ;
char _move_insert_mode ;
char _move_standout_mode ;
char _needs_xon_xoff ;
char _no_esc_ctlc ;
char _non_dest_scroll_region ;
char _non_rev_rmcup ;
char _no_pad_char ;
char _over_strike ;
char _prtr_silent ;
char _row_addr_glitch ;
char _semi_auto_right_margin ;
char _status_line_esc_ok ;
char _tilde_glitch ;
char _transparent_underline ;
char _xon_xoff ;
char _bool_terminator ;
short _numeric_starter ;
short _bit_image_entwining ;
short _bit_image_type ;
short _buffer_capacity ;
short _buttons ;
short _columns ;
short _dot_vert_spacing ;
short _dot_horz_spacing ;
short _init_tabs ;
short _label_height ;
short _label_width ;
short _lines ;
short _lines_of_memory ;
short _magic_cookie_glitch ;
short _max_colors ;
short _max_micro_address ;
short _max_micro_jump ;
short _max_pairs ;
short _maximum_window ;
short _micro_col_size ;
short _micro_line_size ;
short _no_color_video ;
short _number_of_pins ;
short _num_labels ;
short _output_res_char ;
short _outpt_res_line ;
short _output_res_horz_inch ;
short _output_res_vert_inch ;
short _padding_baud_rate ;
short _print_rate ;
short _virtual_terminal ;
short _wide_char_size ;
short _width_status_line ;
short _numeric_terminator ;
char * _string_starter ;
char * _acs_chars ;
char * _alt_scancode_esc ;
char * _back_tab ;
char * _bell ;
char * _bit_image_repeat ;
char * _bit_image_newline ;
char * _carriage_return ;
char * _change_char_pitch ;
char * _change_line_pitch ;
char * _change_res_horz ;
char * _change_res_vert ;
char * _change_scroll_region ;
char * _char_padding ;
char * _char_set_names ;
char * _clear_all_tabs ;
char * _clear_margins ;
char * _clear_screen ;
char * _clr_bol ;
char * _clr_eol ;
char * _clr_eos ;
char * _code_set_init ;
char * _color_names ;
char * _column_address ;
char * _command_character ;
char * _create_window ;
char * _cursor_address ;
char * _cursor_down ;
char * _cursor_home ;
char * _cursor_invisible ;
char * _cursor_left ;
char * _cursor_mem_address ;
char * _cursor_normal ;
char * _cursor_right ;
char * _cursor_to_ll ;
char * _cursor_up ;
char * _cursor_visible ;
char * _define_bit_image_region ;
char * _define_char ;
char * _delete_character ;
char * _delete_line ;
char * _device_type ;
char * _dial_phone ;
char * _dis_status_line ;
char * _display_clock ;
char * _display_pc_char ;
char * _down_half_line ;
char * _ena_acs ;
char * _end_bit_image_region ;
char * _enter_alt_charset_mode ;
char * _enter_am_mode ;
char * _enter_blink_mode ;
char * _enter_bold_mode ;
char * _enter_ca_mode ;
char * _enter_delete_mode ;
char * _enter_dim_mode ;
char * _enter_doublewidth_mode ;
char * _enter_draft_quality ;
char * _enter_insert_mode ;
char * _enter_italics_mode ;
char * _enter_leftward_mode ;
char * _enter_micro_mode ;
char * _enter_near_letter_quality ;
char * _enter_normal_quality ;
char * _enter_pc_charset_mode ;
char * _enter_protected_mode ;
char * _enter_reverse_mode ;
char * _enter_scancode_mode ;
char * _enter_secure_mode ;
char * _enter_shadow_mode ;
char * _enter_standout_mode ;
char * _enter_subscript_mode ;
char * _enter_superscript_mode ;
char * _enter_underline_mode ;
char * _enter_upward_mode ;
char * _enter_xon_mode ;
char * _erase_chars ;
char * _exit_alt_charset_mode ;
char * _exit_am_mode ;
char * _exit_attribute_mode ;
char * _exit_ca_mode ;
char * _exit_delete_mode ;
char * _exit_doublewidth_mode ;
char * _exit_insert_mode ;
char * _exit_italics_mode ;
char * _exit_leftward_mode ;
char * _exit_micro_mode ;
char * _exit_pc_charset_mode ;
char * _exit_scancode_mode ;
char * _exit_shadow_mode ;
char * _exit_standout_mode ;
char * _exit_subscript_mode ;
char * _exit_underline_mode ;
char * _exit_upward_mode ;
char * _exit_xon_mode ;
char * _flash_hook ;
char * _flash_screen ;
char * _fixed_pause ;
char * _form_feed ;
char * _from_status_line ;
char * _get_mouse ;
char * _goto_window ;
char * _hangup ;
char * _init_1string ;
char * _init_2string ;
char * _init_3string ;
char * _init_file ;
char * _init_prog ;
char * _initialize_color ;
char * _initialize_pair ;
char * _insert_character ;
char * _insert_line ;
char * _insert_padding ;
char * _keypad_local ;
char * _keypad_xmit ;
char * _lab_f0 ;
char * _lab_f1 ;
char * _lab_f2 ;
char * _lab_f3 ;
char * _lab_f4 ;
char * _lab_f5 ;
char * _lab_f6 ;
char * _lab_f7 ;
char * _lab_f8 ;
char * _lab_f9 ;
char * _lab_f10 ;
char * _label_format ;
char * _label_off ;
char * _label_on ;
char * _meta_off ;
char * _meta_on ;
char * _micro_column_address ;
char * _micro_down ;
char * _micro_left ;
char * _micro_right ;
char * _micro_row_address ;
char * _micro_up ;
char * _mouse_info ;
char * _newline ;
char * _order_of_pins ;
char * _orig_colors ;
char * _orig_pair ;
char * _pad_char ;
char * _parm_dch ;
char * _parm_delete_line ;
char * _parm_down_cursor ;
char * _parm_down_micro ;
char * _parm_ich ;
char * _parm_index ;
char * _parm_insert_line ;
char * _parm_left_cursor ;
char * _parm_left_micro ;
char * _parm_right_cursor ;
char * _parm_right_micro ;
char * _parm_rindex ;
char * _parm_up_cursor ;
char * _parm_up_micro ;
char * _pc_term_options ;
char * _pkey_key ;
char * _pkey_local ;
char * _pkey_plab ;
char * _pkey_xmit ;
char * _plab_norm ;
char * _print_screen ;
char * _prtr_non ;
char * _prtr_off ;
char * _prtr_on ;
char * _pulse ;
char * _quick_dial ;
char * _remove_clock ;
char * _repeat_char ;
char * _req_for_input ;
char * _req_mouse_pos ;
char * _reset_1string ;
char * _reset_2string ;
char * _reset_3string ;
char * _reset_file ;
char * _restore_cursor ;
char * _row_address ;
char * _save_cursor ;
char * _scancode_escape ;
char * _scroll_forward ;
char * _scroll_reverse ;
char * _select_char_set ;
char * _set0_des_seq ;
char * _set1_des_seq ;
char * _set2_des_seq ;
char * _set3_des_seq ;
char * _set_a_background ;
char * _set_a_foreground ;
char * _set_attributes ;
char * _set_background ;
char * _set_bottom_margin ;
char * _set_bottom_margin_parm ;
char * _set_clock ;
char * _set_color_band ;
char * _set_color_pair ;
char * _set_foreground ;
char * _set_left_margin ;
char * _set_left_margin_parm ;
char * _set_lr_margin ;
char * _set_page_length ;
char * _set_right_margin ;
char * _set_right_margin_parm ;
char * _set_tab ;
char * _set_tb_margin ;
char * _set_top_margin ;
char * _set_top_margin_parm ;
char * _set_window ;
char * _start_bit_image ;
char * _start_char_set_def ;
char * _stop_bit_image ;
char * _stop_char_set_dir ;
char * _subscript_characters ;
char * _superscript_characters ;
char * _tab ;
char * _these_cause_cr ;
char * _to_status_line ;
char * _tone ;
char * _underline_char ;
char * _up_half_line ;
char * _user0 ;
char * _user1 ;
char * _user2 ;
char * _user3 ;
char * _user4 ;
char * _user5 ;
char * _user6 ;
char * _user7 ;
char * _user8 ;
char * _user9 ;
char * _wait_tone ;
char * _xoff_character ;
char * _xon_character ;
char * _zero_motion ;
char * _key_break ;
char * _key_down ;
char * _key_up ;
char * _key_left ;
char * _key_right ;
char * _key_home ;
char * _key_backspace ;
char * _key_f0 ;
char * _key_f1 ;
char * _key_f2 ;
char * _key_f3 ;
char * _key_f4 ;
char * _key_f5 ;
char * _key_f6 ;
char * _key_f7 ;
char * _key_f8 ;
char * _key_f9 ;
char * _key_f10 ;
char * _key_f11 ;
char * _key_f12 ;
char * _key_f13 ;
char * _key_f14 ;
char * _key_f15 ;
char * _key_f16 ;
char * _key_f17 ;
char * _key_f18 ;
char * _key_f19 ;
char * _key_f20 ;
char * _key_f21 ;
char * _key_f22 ;
char * _key_f23 ;
char * _key_f24 ;
char * _key_f25 ;
char * _key_f26 ;
char * _key_f27 ;
char * _key_f28 ;
char * _key_f29 ;
char * _key_f30 ;
char * _key_f31 ;
char * _key_f32 ;
char * _key_f33 ;
char * _key_f34 ;
char * _key_f35 ;
char * _key_f36 ;
char * _key_f37 ;
char * _key_f38 ;
char * _key_f39 ;
char * _key_f40 ;
char * _key_f41 ;
char * _key_f42 ;
char * _key_f43 ;
char * _key_f44 ;
char * _key_f45 ;
char * _key_f46 ;
char * _key_f47 ;
char * _key_f48 ;
char * _key_f49 ;
char * _key_f50 ;
char * _key_f51 ;
char * _key_f52 ;
char * _key_f53 ;
char * _key_f54 ;
char * _key_f55 ;
char * _key_f56 ;
char * _key_f57 ;
char * _key_f58 ;
char * _key_f59 ;
char * _key_f60 ;
char * _key_f61 ;
char * _key_f62 ;
char * _key_f63 ;
char * _key_dl ;
char * _key_il ;
char * _key_dc ;
char * _key_ic ;
char * _key_eic ;
char * _key_clear ;
char * _key_eos ;
char * _key_eol ;
char * _key_sf ;
char * _key_sr ;
char * _key_npage ;
char * _key_ppage ;
char * _key_stab ;
char * _key_ctab ;
char * _key_catab ;
char * _key_enter ;
char * _key_sreset ;
char * _key_reset ;
char * _key_print ;
char * _key_ll ;
char * _key_a1 ;
char * _key_a3 ;
char * _key_b2 ;
char * _key_c1 ;
char * _key_c3 ;
char * _key_btab ;
char * _key_beg ;
char * _key_cancel ;
char * _key_close ;
char * _key_command ;
char * _key_copy ;
char * _key_create ;
char * _key_end ;
char * _key_exit ;
char * _key_find ;
char * _key_help ;
char * _key_mark ;
char * _key_message ;
char * _key_move ;
char * _key_next ;
char * _key_open ;
char * _key_options ;
char * _key_previous ;
char * _key_redo ;
char * _key_reference ;
char * _key_refresh ;
char * _key_replace ;
char * _key_restart ;
char * _key_resume ;
char * _key_save ;
char * _key_sbeg ;
char * _key_scancel ;
char * _key_scommand ;
char * _key_scopy ;
char * _key_screate ;
char * _key_sdc ;
char * _key_sdl ;
char * _key_select ;
char * _key_send ;
char * _key_seol ;
char * _key_sexit ;
char * _key_sfind ;
char * _key_shelp ;
char * _key_shome ;
char * _key_sic ;
char * _key_sleft ;
char * _key_smessage ;
char * _key_smove ;
char * _key_snext ;
char * _key_soptions ;
char * _key_sprevious ;
char * _key_sprint ;
char * _key_sredo ;
char * _key_sreplace ;
char * _key_sright ;
char * _key_srsume ;
char * _key_ssave ;
char * _key_ssupend ;
char * _key_sundo ;
char * _key_suspend ;
char * _key_undo ;
char * _key_mouse ;
char * _string_terminator ;
#ifndef SMALL_CURSES
CONTENT *_colour_content;
PAIR *_colour_pairs;
#endif
int _outfd;
long _flags;
chtype _attr;
chtype _supported_attr;
struct termios _prog_mode;
struct termios _shell_mode;
} TERMINAL ;
#ifdef __STDC__
#define _P_(t) t
#else
#define _P_(t) ()
#endif
extern TERMINAL *cur_term;
extern struct termios _tty_state;
extern char *boolnames[];
extern char *boolcodes[];
extern char *boolfnames[];
extern char *numnames[];
extern char *numcodes[];
extern char *numfnames[];
extern char *strnames[];
extern char *strcodes[];
extern char *strfnames[];
extern int setupterm _P_((char *term,int fildes,int *errret));
#ifndef setterm
extern int setterm _P_((char *term));
#endif
extern int set_curterm _P_((TERMINAL *nterm));
extern int del_curterm _P_((TERMINAL *oterm));
extern int restartterm _P_((char * term,int fildes,int *errret));
extern char *tparm _P_((char *str,...));
extern int tputs _P_((char *str,int count,int (*)(int)));
extern int putp _P_((char *str));
extern int vidputs _P_((chtype attr,int (*)(int)));
extern int vidattr _P_((chtype attr));
extern int mvcur _P_((int oldrow,int oldcol,int newrow,int newcol));
extern int tigetflag _P_((char *capname));
extern int tigetnum _P_((char *capname));
extern char *tigetstr _P_((char *capname));
extern int tgetent _P_((char *bp,char *name));
extern int tgetflag _P_((char *codename));
extern int tgetnum _P_((char *codename));
extern char *tgetstr _P_((char *codename,char **area));
extern char *tgoto _P_((char *codename,int col,int row));
#undef _P_
#endif
