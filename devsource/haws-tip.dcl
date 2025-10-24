haws_tip : dialog {
  label = "Tip";
  : text { key = "tip_msg"; width = 50; height = 20; }
  : row {
    : toggle { key = "opt_in"; label = "Keep showing this tip"; value = "1"; }
  }
  ok_cancel;
}
