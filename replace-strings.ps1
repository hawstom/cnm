# String Replacement Script
# Usage: .\replace-strings.ps1 -FilePath "path\to\file.txt"

param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath
)

# Define your replacements here as an array of hashtables
# Each hashtable has 'Old' and 'New' keys
$replacements = @(
    @{ Old = 'wrap_exceeded_p'; New = 'wrap-exceeded-p' }
    @{ Old = 'word_provided_p'; New = 'word-provided-p' }
    @{ Old = 'vlaobj_property_new'; New = 'vlaobj-property-new' }
    @{ Old = 'vlaobj_block_old'; New = 'vlaobj-block-old' }
    @{ Old = 'vlaobj_block_new'; New = 'vlaobj-block-new' }
    @{ Old = 'valid_editors'; New = 'valid-editors' }
    @{ Old = 'type_def'; New = 'type-def' }
    @{ Old = 'txt2_raw'; New = 'txt2-raw' }
    @{ Old = 'txt2_postfix'; New = 'txt2-postfix' }
    @{ Old = 'txt2_formatted'; New = 'txt2-formatted' }
    @{ Old = 'txt2_auto'; New = 'txt2-auto' }
    @{ Old = 'txt2_attr'; New = 'txt2-attr' }
    @{ Old = 'txt1_raw'; New = 'txt1-raw' }
    @{ Old = 'txt1_postfix'; New = 'txt1-postfix' }
    @{ Old = 'txt1_formatted'; New = 'txt1-formatted' }
    @{ Old = 'txt1_auto'; New = 'txt1-auto' }
    @{ Old = 'txt1_attr'; New = 'txt1-attr' }
    @{ Old = 'transform_data'; New = 'transform-data' }
    @{ Old = 'thisfile_value'; New = 'thisfile-value' }
    @{ Old = 'tag_list'; New = 'tag-list' }
    @{ Old = 'tag_data'; New = 'tag-data' }
    @{ Old = 'split_on_nbsp'; New = 'split-on-nbsp' }
    @{ Old = 'slope_value'; New = 'slope-value' }
    @{ Old = 'slope_percent'; New = 'slope-percent' }
    @{ Old = 'skip_entry_p'; New = 'skip-entry-p' }
    @{ Old = 'sheet_quantities'; New = 'sheet-quantities' }
    @{ Old = 'sheet_list_line'; New = 'sheet-list-line' }
    @{ Old = 'sheet_list_filename'; New = 'sheet-list-filename' }
    @{ Old = 'sheet_headings'; New = 'sheet-headings' }
    @{ Old = 'sheet_filenames'; New = 'sheet-filenames' }
    @{ Old = 'sheet_filename'; New = 'sheet-filename' }
    @{ Old = 'sheet_file_name'; New = 'sheet-file-name' }
    @{ Old = 'setvar_p'; New = 'setvar-p' }
    @{ Old = 'scope_list'; New = 'scope-list' }
    @{ Old = 'scope_key'; New = 'scope-key' }
    @{ Old = 'scope_code'; New = 'scope-code' }
    @{ Old = 'return_list'; New = 'return-list' }
    @{ Old = 'requires_coordinates'; New = 'requires-coordinates' }
    @{ Old = 'requested_format'; New = 'requested-format' }
    @{ Old = 'replace_bubble_p'; New = 'replace-bubble-p' }
    @{ Old = 'reg_val'; New = 'reg-val' }
    @{ Old = 'reference_type'; New = 'reference-type' }
    @{ Old = 'reference_list'; New = 'reference-list' }
    @{ Old = 'ref_wcs_3'; New = 'ref-wcs-3' }
    @{ Old = 'ref_wcs_2'; New = 'ref-wcs-2' }
    @{ Old = 'ref_wcs_1'; New = 'ref-wcs-1' }
    @{ Old = 'ref_ocs_3'; New = 'ref-ocs-3' }
    @{ Old = 'ref_ocs_2'; New = 'ref-ocs-2' }
    @{ Old = 'ref_ocs_1'; New = 'ref-ocs-1' }
    @{ Old = 'reactors_old'; New = 'reactors-old' }
    @{ Old = 'reactor_old'; New = 'reactor-old' }
    @{ Old = 'reactor_count'; New = 'reactor-count' }
    @{ Old = 'qty_string'; New = 'qty-string' }
    @{ Old = 'pspace_current_p'; New = 'pspace-current-p' }
    @{ Old = 'pspace_bubble_p'; New = 'pspace-bubble-p' }
    @{ Old = 'project_folder'; New = 'project-folder' }
    @{ Old = 'project_file_name'; New = 'project-file-name' }
    @{ Old = 'phases_definition'; New = 'phases-definition' }
    @{ Old = 'pgp_filename'; New = 'pgp-filename' }
    @{ Old = 'pgp_file_line'; New = 'pgp-file-line' }
    @{ Old = 'pgp_file_contents'; New = 'pgp-file-contents' }
    @{ Old = 'pgp_defines_run'; New = 'pgp-defines-run' }
    @{ Old = 'p1_world'; New = 'p1-world' }
    @{ Old = 'p1_ucs'; New = 'p1-ucs' }
    @{ Old = 'p1_ocs'; New = 'p1-ocs' }
    @{ Old = 'p_ocs'; New = 'p-ocs' }
    @{ Old = 'on_model_tab_p'; New = 'on-model-tab-p' }
    @{ Old = 'old_filename'; New = 'old-filename' }
    @{ Old = 'offset_value'; New = 'offset-value' }
    @{ Old = 'object_leader'; New = 'object-leader' }
    @{ Old = 'obj_reactor'; New = 'obj-reactor' }
    @{ Old = 'obj_pipe'; New = 'obj-pipe' }
    @{ Old = 'obj_notifier'; New = 'obj-notifier' }
    @{ Old = 'obj_next'; New = 'obj-next' }
    @{ Old = 'obj_bubble_temp'; New = 'obj-bubble-temp' }
    @{ Old = 'note_first_line_p'; New = 'note-first-line-p' }
    @{ Old = 'new_prefix'; New = 'new-prefix' }
    @{ Old = 'new_postfix'; New = 'new-postfix' }
    @{ Old = 'new_filename'; New = 'new-filename' }
    @{ Old = 'new_extension'; New = 'new-extension' }
    @{ Old = 'need_wrap_p'; New = 'need-wrap-p' }
    @{ Old = 'mark_file_p'; New = 'mark-file-p' }
    @{ Old = 'local_project_marker'; New = 'local-project-marker' }
    @{ Old = 'local_project_folder'; New = 'local-project-folder' }
    @{ Old = 'local_marker_file'; New = 'local-marker-file' }
    @{ Old = 'linked_project_marker'; New = 'linked-project-marker' }
    @{ Old = 'linked_project_folder'; New = 'linked-project-folder' }
    @{ Old = 'link_marker'; New = 'link-marker' }
    @{ Old = 'length_value'; New = 'length-value' }
    @{ Old = 'layout_name'; New = 'layout-name' }
    @{ Old = 'key_app'; New = 'key-app' }
    @{ Old = 'inword_p'; New = 'inword-p' }
    @{ Old = 'input_key'; New = 'input-key' }
    @{ Old = 'i_title'; New = 'i-title' }
    @{ Old = 'i_newword_prev'; New = 'i-newword-prev' }
    @{ Old = 'i_newline_prev'; New = 'i-newline-prev' }
    @{ Old = 'i_newline'; New = 'i-newline' }
    @{ Old = 'i_endline'; New = 'i-endline' }
    @{ Old = 'hcnm-wrap_description_test'; New = 'hcnm-wrap-description-test' }
    @{ Old = 'hcnm-wrap_description'; New = 'hcnm-wrap-description' }
    @{ Old = 'hcnm-shorten_path'; New = 'hcnm-shorten-path' }
    @{ Old = 'hcnm-set_tile_list'; New = 'hcnm-set-tile-list' }
    @{ Old = 'hcnm-set_dimstyle'; New = 'hcnm-set-dimstyle' }
    @{ Old = 'hcnm-set_attributes'; New = 'hcnm-set-attributes' }
    @{ Old = 'hcnm-restore_dimstyle'; New = 'hcnm-restore-dimstyle' }
    @{ Old = 'hcnm-projnotes_match_extension'; New = 'hcnm-projnotes-match-extension' }
    @{ Old = 'hcnm-project_link_name'; New = 'hcnm-project-link-name' }
    @{ Old = 'hcnm-project_ini_name'; New = 'hcnm-project-ini-name' }
    @{ Old = 'hcnm-project_folder_to_link'; New = 'hcnm-project-folder-to-link' }
    @{ Old = 'hcnm-project_folder_to_ini'; New = 'hcnm-project-folder-to-ini' }
    @{ Old = 'hcnm-options_list_data'; New = 'hcnm-options-list-data' }
    @{ Old = 'hcnm-local_project_marker'; New = 'hcnm-local-project-marker' }
    @{ Old = 'hcnm-linked_project_marker'; New = 'hcnm-linked-project-marker' }
    @{ Old = 'hcnm-ldrblk_warn_pspace_coordinates'; New = 'hcnm-ldrblk-warn-pspace-coordinates' }
    @{ Old = 'hcnm-ldrblk_update_bubble_tag'; New = 'hcnm-ldrblk-update-bubble-tag' }
    @{ Old = 'hcnm-ldrblk_space_set_model'; New = 'hcnm-ldrblk-space-set-model' }
    @{ Old = 'hcnm-ldrblk_space_restore'; New = 'hcnm-ldrblk-space-restore' }
    @{ Old = 'hcnm-ldrblk_set_viewport_transform_xdata'; New = 'hcnm-ldrblk-set-viewport-transform-xdata' }
    @{ Old = 'hcnm-ldrblk_set_dynprops'; New = 'hcnm-ldrblk-set-dynprops' }
    @{ Old = 'hcnm-ldrblk_set_avport_xdata'; New = 'hcnm-ldrblk-set-avport-xdata' }
    @{ Old = 'hcnm-ldrblk_save_auto_to_list'; New = 'hcnm-ldrblk-save-auto-to-list' }
    @{ Old = 'hcnm-ldrblk_save_attribute_to_list'; New = 'hcnm-ldrblk-save-attribute-to-list' }
    @{ Old = 'hcnm-ldrblk_reactor_update'; New = 'hcnm-ldrblk-reactor-update' }
    @{ Old = 'hcnm-ldrblk_reactor_callback'; New = 'hcnm-ldrblk-reactor-callback' }
    @{ Old = 'hcnm-ldrblk_reactor'; New = 'hcnm-ldrblk-reactor' }
    @{ Old = 'hcnm-ldrblk_p1_world'; New = 'hcnm-ldrblk-p1-world' }
    @{ Old = 'hcnm-ldrblk_p1_ocs'; New = 'hcnm-ldrblk-p1-ocs' }
    @{ Old = 'hcnm-ldrblk_mtextatribute_p'; New = 'hcnm-ldrblk-mtextatribute-p' }
    @{ Old = 'hcnm-ldrblk_list_reactors'; New = 'hcnm-ldrblk-list-reactors' }
    @{ Old = 'hcnm-ldrblk_is_on_model_tab'; New = 'hcnm-ldrblk-is-on-model-tab' }
    @{ Old = 'hcnm-ldrblk_initialize_attribute_list'; New = 'hcnm-ldrblk-initialize-attribute-list' }
    @{ Old = 'hcnm-ldrblk_get_viewport_transform_xdata'; New = 'hcnm-ldrblk-get-viewport-transform-xdata' }
    @{ Old = 'hcnm-ldrblk_get_user_start_point'; New = 'hcnm-ldrblk-get-user-start-point' }
    @{ Old = 'hcnm-ldrblk_get_text_entry'; New = 'hcnm-ldrblk-get-text-entry' }
    @{ Old = 'hcnm-ldrblk_get_target_vport'; New = 'hcnm-ldrblk-get-target-vport' }
    @{ Old = 'hcnm-ldrblk_get_p2_data'; New = 'hcnm-ldrblk-get-p2-data' }
    @{ Old = 'hcnm-ldrblk_get_mtext_string'; New = 'hcnm-ldrblk-get-mtext-string' }
    @{ Old = 'hcnm-ldrblk_get_ename_bubble_old'; New = 'hcnm-ldrblk-get-ename-bubble-old' }
    @{ Old = 'hcnm-ldrblk_get_bubble_data'; New = 'hcnm-ldrblk-get-bubble-data' }
    @{ Old = 'hcnm-ldrblk_get_avport_xdata'; New = 'hcnm-ldrblk-get-avport-xdata' }
    @{ Old = 'hcnm-ldrblk_get_auto_type_keys'; New = 'hcnm-ldrblk-get-auto-type-keys' }
    @{ Old = 'hcnm-ldrblk_get_auto_type'; New = 'hcnm-ldrblk-get-auto-type' }
    @{ Old = 'hcnm-ldrblk_finish_bubble'; New = 'hcnm-ldrblk-finish-bubble' }
    @{ Old = 'hcnm-ldrblk_ensure_fields_and_adjust_formats'; New = 'hcnm-ldrblk-ensure-fields-and-adjust-formats' }
    @{ Old = 'hcnm-ldrblk_ensure_fields'; New = 'hcnm-ldrblk-ensure-fields' }
    @{ Old = 'hcnm-ldrblk_ensure_field'; New = 'hcnm-ldrblk-ensure-field' }
    @{ Old = 'hcnm-ldrblk_dynamic'; New = 'hcnm-ldrblk-dynamic' }
    @{ Old = 'hcnm-ldrblk_draw_bubble'; New = 'hcnm-ldrblk-draw-bubble' }
    @{ Old = 'hcnm-ldrblk_count_char'; New = 'hcnm-ldrblk-count-char' }
    @{ Old = 'hcnm-ldrblk_clear_viewport_transform_xdata'; New = 'hcnm-ldrblk-clear-viewport-transform-xdata' }
    @{ Old = 'hcnm-ldrblk_change_arrowhead'; New = 'hcnm-ldrblk-change-arrowhead' }
    @{ Old = 'hcnm-ldrblk_capture_viewport_transform'; New = 'hcnm-ldrblk-capture-viewport-transform' }
    @{ Old = 'hcnm-ldrblk_bubble_leader'; New = 'hcnm-ldrblk-bubble-leader' }
    @{ Old = 'hcnm-ldrblk_bubble_has_leader'; New = 'hcnm-ldrblk-bubble-has-leader' }
    @{ Old = 'hcnm-ldrblk_bd_set'; New = 'hcnm-ldrblk-bd-set' }
    @{ Old = 'hcnm-ldrblk_bd_get'; New = 'hcnm-ldrblk-bd-get' }
    @{ Old = 'hcnm-ldrblk_bd_ensure_p1_world'; New = 'hcnm-ldrblk-bd-ensure-p1-world' }
    @{ Old = 'hcnm-ldrblk_bd_def'; New = 'hcnm-ldrblk-bd-def' }
    @{ Old = 'hcnm-ldrblk_bd'; New = 'hcnm-ldrblk-bd' }
    @{ Old = 'hcnm-ldrblk_auto_update'; New = 'hcnm-ldrblk-auto-update' }
    @{ Old = 'hcnm-ldrblk_auto_type_is_coordinate_p'; New = 'hcnm-ldrblk-auto-type-is-coordinate-p' }
    @{ Old = 'hcnm-ldrblk_auto_rtos'; New = 'hcnm-ldrblk-auto-rtos' }
    @{ Old = 'hcnm-ldrblk_auto_pipe_get_object'; New = 'hcnm-ldrblk-auto-pipe-get-object' }
    @{ Old = 'hcnm-ldrblk_auto_pipe_format_slope'; New = 'hcnm-ldrblk-auto-pipe-format-slope' }
    @{ Old = 'hcnm-ldrblk_auto_pipe_format_length'; New = 'hcnm-ldrblk-auto-pipe-format-length' }
    @{ Old = 'hcnm-ldrblk_auto_pipe_format_diameter'; New = 'hcnm-ldrblk-auto-pipe-format-diameter' }
    @{ Old = 'hcnm-ldrblk_auto_pipe'; New = 'hcnm-ldrblk-auto-pipe' }
    @{ Old = 'hcnm-ldrblk_auto_get_input'; New = 'hcnm-ldrblk-auto-get-input' }
    @{ Old = 'hcnm-ldrblk_auto_dispatch'; New = 'hcnm-ldrblk-auto-dispatch' }
    @{ Old = 'hcnm-ldrblk_auto_apology'; New = 'hcnm-ldrblk-auto-apology' }
    @{ Old = 'hcnm-ldrblk_auto_alignment_format_station'; New = 'hcnm-ldrblk-auto-alignment-format-station' }
    @{ Old = 'hcnm-ldrblk_auto_alignment_format_offset'; New = 'hcnm-ldrblk-auto-alignment-format-offset' }
    @{ Old = 'hcnm-ldrblk_auto_alignment_calculate'; New = 'hcnm-ldrblk-auto-alignment-calculate' }
    @{ Old = 'hcnm-ldrblk_auto_al'; New = 'hcnm-ldrblk-auto-al' }
    @{ Old = 'hcnm-ldrblk_attr_has_content_p'; New = 'hcnm-ldrblk-attr-has-content-p' }
    @{ Old = 'hcnm-ldrblk_assure_auto_text_has_reactor'; New = 'hcnm-ldrblk-assure-auto-text-has-reactor' }
    @{ Old = 'hcnm-ldrblk_apply_transform_matrix'; New = 'hcnm-ldrblk-apply-transform-matrix' }
    @{ Old = 'hcnm-ldrblk_adjust_notedata'; New = 'hcnm-ldrblk-adjust-notedata' }
    @{ Old = 'hcnm-ldrblk_adjust_formats'; New = 'hcnm-ldrblk-adjust-formats' }
    @{ Old = 'hcnm-ldrblk_adjust_format'; New = 'hcnm-ldrblk-adjust-format' }
    @{ Old = 'hcnm-key_table_searchandsave'; New = 'hcnm-key-table-searchandsave' }
    @{ Old = 'hcnm-key_table_make'; New = 'hcnm-key-table-make' }
    @{ Old = 'hcnm-key_table_insert_text9'; New = 'hcnm-key-table-insert-text9' }
    @{ Old = 'hcnm-key_table_insert_text'; New = 'hcnm-key-table-insert-text' }
    @{ Old = 'hcnm-key_table_insert_shape9'; New = 'hcnm-key-table-insert-shape9' }
    @{ Old = 'hcnm-key_table_insert_shape'; New = 'hcnm-key-table-insert-shape' }
    @{ Old = 'hcnm-key_table_from_search'; New = 'hcnm-key-table-from-search' }
    @{ Old = 'hcnm-key_table_advance_down'; New = 'hcnm-key-table-advance-down' }
    @{ Old = 'hcnm-initialize_project'; New = 'hcnm-initialize-project' }
    @{ Old = 'hcnm-ini_name'; New = 'hcnm-ini-name' }
    @{ Old = 'hcnm-get_attributes'; New = 'hcnm-get-attributes' }
    @{ Old = 'hcnm-error_not_writeable'; New = 'hcnm-error-not-writeable' }
    @{ Old = 'hcnm-error_ambiguous_project_markers'; New = 'hcnm-error-ambiguous-project-markers' }
    @{ Old = 'hcnm-edit_bubble_done_codes'; New = 'hcnm-edit-bubble-done-codes' }
    @{ Old = 'hcnm-edit_bubble_cancel'; New = 'hcnm-edit-bubble-cancel' }
    @{ Old = 'hcnm-edit_bubble'; New = 'hcnm-edit-bubble' }
    @{ Old = 'hcnm-eb-update_prefix'; New = 'hcnm-eb-update-prefix' }
    @{ Old = 'hcnm-eb-update_postfix'; New = 'hcnm-eb-update-postfix' }
    @{ Old = 'hcnm-eb-split_string'; New = 'hcnm-eb-split-string' }
    @{ Old = 'hcnm-eb-split_on_nbsp'; New = 'hcnm-eb-split-on-nbsp' }
    @{ Old = 'hcnm-eb-remove_delimiters'; New = 'hcnm-eb-remove-delimiters' }
    @{ Old = 'hcnm-eb-migrate_legacy_format'; New = 'hcnm-eb-migrate-legacy-format' }
    @{ Old = 'hcnm-eb-get_text'; New = 'hcnm-eb-get-text' }
    @{ Old = 'hcnm-eb-flatten_value'; New = 'hcnm-eb-flatten-value' }
    @{ Old = 'hcnm-eb-expand_value_to_delimited'; New = 'hcnm-eb-expand-value-to-delimited' }
    @{ Old = 'hcnm-eb-concat_parts'; New = 'hcnm-eb-concat-parts' }
    @{ Old = 'hcnm-eb-attribute_list'; New = 'hcnm-eb-attribute-list' }
    @{ Old = 'hcnm-eb-add_delimiters'; New = 'hcnm-eb-add-delimiters' }
    @{ Old = 'hcnm-debug_show_bubble_reactor_xdata'; New = 'hcnm-debug-show-bubble-reactor-xdata' }
    @{ Old = 'hcnm-dcl_qt_show'; New = 'hcnm-dcl-qt-show' }
    @{ Old = 'hcnm-dcl_options_show'; New = 'hcnm-dcl-options-show' }
    @{ Old = 'hcnm-dcl_options_save'; New = 'hcnm-dcl-options-save' }
    @{ Old = 'hcnm-dcl_options_cancel'; New = 'hcnm-dcl-options-cancel' }
    @{ Old = 'hcnm-dcl_options_cancel'; New = 'hcnm-dcl-options-cancel' }
    @{ Old = 'hcnm-dcl_key_show'; New = 'hcnm-dcl-key-show' }
    @{ Old = 'hcnm-dcl_general_show'; New = 'hcnm-dcl-general-show' }
    @{ Old = 'hcnm-dcl_bubble_show'; New = 'hcnm-dcl-bubble-show' }
    @{ Old = 'hcnm-config_write_user'; New = 'hcnm-config-write-user' }
    @{ Old = 'hcnm-config_write_session'; New = 'hcnm-config-write-session' }
    @{ Old = 'hcnm-config_write_project'; New = 'hcnm-config-write-project' }
    @{ Old = 'hcnm-config_temp_setvar'; New = 'hcnm-config-temp-setvar' }
    @{ Old = 'hcnm-config_temp_setvar'; New = 'hcnm-config-temp-setvar' }
    @{ Old = 'hcnm-config_temp_save'; New = 'hcnm-config-temp-save' }
    @{ Old = 'hcnm-config_temp_getvar'; New = 'hcnm-config-temp-getvar' }
    @{ Old = 'hcnm-config_temp_clear'; New = 'hcnm-config-temp-clear' }
    @{ Old = 'hcnm-config_set_action_tile'; New = 'hcnm-config-set-action-tile' }
    @{ Old = 'hcnm-config_scope_eq'; New = 'hcnm-config-scope-eq' }
    @{ Old = 'hcnm-config_scope_code'; New = 'hcnm-config-scope-code' }
    @{ Old = 'hcnm-config_read_user'; New = 'hcnm-config-read-user' }
    @{ Old = 'hcnm-config_read_session'; New = 'hcnm-config-read-session' }
    @{ Old = 'hcnm-config_read_all_user'; New = 'hcnm-config-read-all-user' }
    @{ Old = 'hcnm-config_read_all_session'; New = 'hcnm-config-read-all-session' }
    @{ Old = 'hcnm-config_read_all_project'; New = 'hcnm-config-read-all-project' }
    @{ Old = 'hcnm-config_read_all'; New = 'hcnm-config-read-all' }
    @{ Old = 'hcnm-config_project_notes_format'; New = 'hcnm-config-project-notes-format' }
    @{ Old = 'hcnm-config_get_default'; New = 'hcnm-config-get-default' }
    @{ Old = 'hcnm-config_entry_var'; New = 'hcnm-config-entry-var' }
    @{ Old = 'hcnm-config_entry_val'; New = 'hcnm-config-entry-val' }
    @{ Old = 'hcnm-config_entry_strip_scope'; New = 'hcnm-config-entry-strip-scope' }
    @{ Old = 'hcnm-config_entry_scope_code'; New = 'hcnm-config-entry-scope-code' }
    @{ Old = 'hcnm-config_definitions'; New = 'hcnm-config-definitions' }
    @{ Old = 'hcnm-config_defaults_single_scope'; New = 'hcnm-config-defaults-single-scope' }
    @{ Old = 'hcnm-config_defaults'; New = 'hcnm-config-defaults' }
    @{ Old = 'hcnm-config_default_projectnoteseditor'; New = 'hcnm-config-default-projectnoteseditor' }
    @{ Old = 'hcnm-config_dcl_list_callback'; New = 'hcnm-config-dcl-list-callback' }
    @{ Old = 'hcnm-config_dcl_list'; New = 'hcnm-config-dcl-list' }
    @{ Old = 'hcnm-concept_vartoval'; New = 'hcnm-concept-vartoval' }
    @{ Old = 'hcnm-concept_vartosetting'; New = 'hcnm-concept-vartosetting' }
    @{ Old = 'hcnm-concept_testsetvar'; New = 'hcnm-concept-testsetvar' }
    @{ Old = 'hcnm-concept_testgetvar'; New = 'hcnm-concept-testgetvar' }
    @{ Old = 'hcnm-concept_setvar'; New = 'hcnm-concept-setvar' }
    @{ Old = 'hcnm-concept_savesettingstoini'; New = 'hcnm-concept-savesettingstoini' }
    @{ Old = 'hcnm-concept_removesettings'; New = 'hcnm-concept-removesettings' }
    @{ Old = 'hcnm-concept_inifolder'; New = 'hcnm-concept-inifolder' }
    @{ Old = 'hcnm-concept_inifile'; New = 'hcnm-concept-inifile' }
    @{ Old = 'hcnm-concept_ini'; New = 'hcnm-concept-ini' }
    @{ Old = 'hcnm-concept_getvar'; New = 'hcnm-concept-getvar' }
    @{ Old = 'hcnm-concept_getsettings'; New = 'hcnm-concept-getsettings' }
    @{ Old = 'hcnm-concept_getinidefaults'; New = 'hcnm-concept-getinidefaults' }
    @{ Old = 'hcnm-concept_filename_directory'; New = 'hcnm-concept-filename-directory' }
    @{ Old = 'hcnm-concept_file_copy'; New = 'hcnm-concept-file-copy' }
    @{ Old = 'hcnm-concept_edcgetvar'; New = 'hcnm-concept-edcgetvar' }
    @{ Old = 'hcnm-concept_addvartolist'; New = 'hcnm-concept-addvartolist' }
    @{ Old = 'hcnm-check_moved_project'; New = 'hcnm-check-moved-project' }
    @{ Old = 'hcnm-change_filename_extension'; New = 'hcnm-change-filename-extension' }
    @{ Old = 'hcnm-change_filename_extension'; New = 'hcnm-change-filename-extension' }
    @{ Old = 'hcnm-change_filename_extension'; New = 'hcnm-change-filename-extension' }
    @{ Old = 'hcnm-assure_local_project'; New = 'hcnm-assure-local-project' }
    @{ Old = 'hcnm-assure_linked_project'; New = 'hcnm-assure-linked-project' }
    @{ Old = 'haws_tip_show'; New = 'haws-tip-show' }
    @{ Old = 'haws_nested_list_update'; New = 'haws-nested-list-update' }
    @{ Old = 'haws_nested_list_get'; New = 'haws-nested-list-get' }
    @{ Old = 'haws_evangel_msg'; New = 'haws-evangel-msg' }
    @{ Old = 'haws_cnm_evangel_msg'; New = 'haws-cnm-evangel-msg' }
    @{ Old = 'handle_reference'; New = 'handle-reference' }
    @{ Old = 'handle_notifier'; New = 'handle-notifier' }
    @{ Old = 'handle_leader'; New = 'handle-leader' }
    @{ Old = 'handle_bubble'; New = 'handle-bubble' }
    @{ Old = 'get_auto_type_keys'; New = 'get-auto-type-keys' }
    @{ Old = 'found_p'; New = 'found-p' }
    @{ Old = 'format_code'; New = 'format-code' }
    @{ Old = 'field_code_p'; New = 'field-code-p' }
    @{ Old = 'field_code'; New = 'field-code' }
    @{ Old = 'ensure_fields'; New = 'ensure-fields' }
    @{ Old = 'end_list'; New = 'end-list' }
    @{ Old = 'ename_temp'; New = 'ename-temp' }
    @{ Old = 'ename_replace_bubble_p'; New = 'ename-replace-bubble-p' }
    @{ Old = 'ename_reference'; New = 'ename-reference' }
    @{ Old = 'ename_next'; New = 'ename-next' }
    @{ Old = 'ename_leader_old'; New = 'ename-leader-old' }
    @{ Old = 'ename_leader'; New = 'ename-leader' }
    @{ Old = 'ename_last'; New = 'ename-last' }
    @{ Old = 'ename_bubble_temp'; New = 'ename-bubble-temp' }
    @{ Old = 'ename_bubble_old'; New = 'ename-bubble-old' }
    @{ Old = 'ename_bubble_new'; New = 'ename-bubble-new' }
    @{ Old = 'ename_bubble'; New = 'ename-bubble' }
    @{ Old = 'ename_block'; New = 'ename-block' }
    @{ Old = 'ename_330'; New = 'ename-330' }
    @{ Old = 'elist_no_xdata'; New = 'elist-no-xdata' }
    @{ Old = 'elist_leader_old'; New = 'elist-leader-old' }
    @{ Old = 'elist_leader'; New = 'elist-leader' }
    @{ Old = 'elist_bubble'; New = 'elist-bubble' }
    @{ Old = 'elist_block_old'; New = 'elist-block-old' }
    @{ Old = 'eb_done'; New = 'eb-done' }
    @{ Old = 'dyn_props_old_i'; New = 'dyn-props-old-i' }
    @{ Old = 'dyn_props_old'; New = 'dyn-props-old' }
    @{ Old = 'down_height'; New = 'down-height' }
    @{ Old = 'done_code'; New = 'done-code' }
    @{ Old = 'display_type'; New = 'display-type' }
    @{ Old = 'dia_value'; New = 'dia-value' }
    @{ Old = 'dia_inches'; New = 'dia-inches' }
    @{ Old = 'delim_pos'; New = 'delim-pos' }
    @{ Old = 'define_configs'; New = 'define-configs' }
    @{ Old = 'data_old'; New = 'data-old' }
    @{ Old = 'cvport_stored'; New = 'cvport-stored' }
    @{ Old = 'cvport_old'; New = 'cvport-old' }
    @{ Old = 'count_sep'; New = 'count-sep' }
    @{ Old = 'count_ps'; New = 'count-ps' }
    @{ Old = 'count_ms'; New = 'count-ms' }
    @{ Old = 'count_ldr'; New = 'count-ldr' }
    @{ Old = 'column_height_pending'; New = 'column-height-pending' }
    @{ Old = 'column_height'; New = 'column-height' }
    @{ Old = 'column_height'; New = 'column-height' }
    @{ Old = 'cnmedit_p'; New = 'cnmedit-p' }
    @{ Old = 'character_i'; New = 'character-i' }
    @{ Old = 'bubble_list'; New = 'bubble-list' }
    @{ Old = 'bubble_data'; New = 'bubble-data' }
    @{ Old = 'block_data'; New = 'block-data' }
    @{ Old = 'auto_type'; New = 'auto-type' }
    @{ Old = 'auto_su'; New = 'auto-su' }
    @{ Old = 'auto_string'; New = 'auto-string' }
    @{ Old = 'auto_new'; New = 'auto-new' }
    @{ Old = 'auto_ne'; New = 'auto-ne' }
    @{ Old = 'auto_al'; New = 'auto-al' }
    @{ Old = 'attribute_list_old'; New = 'attribute-list-old' }
    @{ Old = 'attribute_list'; New = 'attribute-list' }
    @{ Old = 'associate_p'; New = 'associate-p' }
    @{ Old = 'all_sheets_quantities'; New = 'all-sheets-quantities' }
    @{ Old = 'alignment_object'; New = 'alignment-object' }
    @{ Old = 'adjust_formats'; New = 'adjust-formats' }
    @{ Old = 'adjust_format'; New = 'adjust-format' }
    @{ Old = '*hcnm-config_temp*'; New = '*hcnm-config-temp*' }
    @{ Old = '*hcnm-config_session*'; New = '*hcnm-config-session*' }
    @{ Old = '*hcnm-config_defaults*'; New = '*hcnm-config-defaults*' }
    @{ Old = '*hcnm-concept_settings*?'; New = '*hcnm-concept-settings*?' }
    @{ Old = '*hcnm-concept_settings*'; New = '*hcnm-concept-settings*' }
    @{ Old = '*hcnm-concept_inifolder*'; New = '*hcnm-concept-inifolder*' }
)

# Check if file exists
if (-not (Test-Path $FilePath)) {
    Write-Error "File not found: $FilePath"
    exit 1
}

# Read the entire file
$content = Get-Content $FilePath -Raw

# Sort replacements by length (longest first) to avoid partial replacements
$replacements = $replacements | Sort-Object { $_.Old.Length } -Descending

# Track if any changes were made
$changesMade = $false
$replacementCount = 0

# Apply each replacement
foreach ($replacement in $replacements) {
    $oldText = $replacement.Old
    $newText = $replacement.New
    
    if ($content -match [regex]::Escape($oldText)) {
        $occurrences = ([regex]::Matches($content, [regex]::Escape($oldText))).Count
        $content = $content -replace [regex]::Escape($oldText), $newText
        Write-Host "Replaced '$oldText' with '$newText' ($occurrences occurrence(s))" -ForegroundColor Green
        $changesMade = $true
        $replacementCount += $occurrences
    } else {
        Write-Host "Not found: '$oldText'" -ForegroundColor Yellow
    }
}

# Save the file if changes were made
if ($changesMade) {
    # Create backup
    $backupPath = "$FilePath.backup"
    Copy-Item $FilePath $backupPath -Force
    Write-Host "`nBackup created: $backupPath" -ForegroundColor Cyan
    
    # Write modified content
    Set-Content $FilePath $content -NoNewline
    Write-Host "File updated: $FilePath" -ForegroundColor Cyan
    Write-Host "Total replacements: $replacementCount" -ForegroundColor Cyan
} else {
    Write-Host "`nNo changes made." -ForegroundColor Yellow
}
