// EDITALL.DCL   dialog box for editing
// dimensioned text.

DDEDIM : dialog
 { label = "Edit Dimension Text";
 : row {
  : radio_column {
   spacer;
    : radio_button {
     key = "USR";
     label = "User Input Text : " ;
     mnemonic = "U"; }
   spacer;
   : radio_button {
    key = "DEF";
    label = "Actual Dimension: ";
    mnemonic = "A"; }
 }
 : column {
  : edit_box {
    key = "USRTXT";
    width = 45;
    allow_accept = true; }
   spacer_1;
   : text { key = "DEFTXT"; }
  spacer;
 }
}
: boxed_column {
 children_alignment = centered;
 width = 65;
 fixed_width = true;
: text {
 label = "NOTE: use <> to embed actual calculated"; }
: text {
 label = "      dimension into user input text   "; }
 }
 spacer_1;
 ok_cancel;
}
