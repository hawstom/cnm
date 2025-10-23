haws_tip : dialog {
  label = "Tip";
  : text { key = "tip_msg"; width = 50; height = 20; }
  : row {
    : toggle { key = "opt_out"; label = "Do not show again"; value = "0"; }
  }
  ok_cancel;
}
