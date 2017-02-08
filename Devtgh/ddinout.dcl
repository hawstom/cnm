DDINOUT : dialog {
  key = "TITLE";
  label = "Large List Select Dialog";
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
  : text {
  	key = "TITLE";
	value = "Not the list you want?  Fix it. INOUT.LSP is Open Source!";
  }
}
