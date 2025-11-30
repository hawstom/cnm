haws_tip : dialog {
  label = "Tip";
  : text { key = "tip_msg"; width = 50; height = 20; }
  spacer;
  : row {
    : text { label = "Snooze this tip for:"; }
    : popup_list {
      key = "snooze_dropdown";
      width = 20;
      list = "No snooze\n1 day\n7 days\n30 days\n180 days\nForever";
      value = "0";
    }
  }
  spacer;
  : row {
//    : spacer { width = 1; }
    : button {    // defines the OK button
      label = "OK";
      is_default = true;
      key = "accept";
      width = 8;
      fixed_width = true;
    }
    : button {    // defines the Cancel button
        label = "Cancel";
        is_cancel = true;
        key = "cancel";
        width = 8;
        fixed_width = true;
    }
//    : spacer { width = 1;}
    : button { 
      key = "resurrect_btn"; 
      label = "Show All Tips"; 
      width = 8; 
    }
  }
}
