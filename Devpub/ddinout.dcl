DDINOUT : dialog {
  label = "Large List Select Dialog";
  key = "TITLE";
  : row {
    : list_box {
      key = "MSL";
      label = "Select xrefs";
      fixed_width = true;
      width = 50;
      alignment = centered;
//      height = 15;
      tabs="1";
    }
    : column {
        fixed_height = true;
        alignment = centered;
        : button {
          key="SEL";
          label="Select All";
        }
        : button {
          key="UNS";
          label="Unselect All";
        }
        ok_button;
        cancel_button;
    }
  }
}
