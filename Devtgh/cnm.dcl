// CNM.DCL
//
//
// CNMOptions dialog
close_button : retirement_button {
  label = "&Close";
  key = "close";
  is_default = true;
}
HCNMOptions : dialog {
	key = "Title";
	label = "";
	: button {label = "&General..."; key = "General";}
	: button {label = "&Key Notes Table..."; key = "Key";}
	: button {label = "&Quantity Take-off Table..."; key = "QT";}
	ok_cancel;
}

HCNMGeneral : dialog {
	key = "Title";
	label = "";
	: boxed_column {
		label = "Scale and size";
		: edit_box {
			key = "NotesLeaderDimstyle";
			label = "Dimstyle to set current (if it exists) when adding bubble notes";
			fixed_width = true;
			edit_width = 16;
		}
		: edit_box {
			key = "NotesKeyTableDimstyle";
			label = "Dimstyle to set current (if it exists) when making tables";
			fixed_width = true;
			edit_width = 16;
		}
	}
	: boxed_column {
		label = "Files and Editors";
		: text {
			key = "ProjectFolder";
			label = "";
		}
		: row {
			: edit_box {
				key = "ProjectNotes";
				label = "Project Notes file";
				edit_width = 32;
			}
			: button {
				key = "ProjectNotesBrowse";
				label = "Browse...";
			}
		}
		: popup_list {
			key = "ProjectNotesEditor";
			label = "Project Notes editor for this project";
		}
		: row {
			: popup_list {
				key = "LayersEditor";
				label = "Layer Settings editor for this user";
			}
		}
	}  
	: boxed_column {
		label = "Bubble Notes";
		: text {label = "CNM never searches layers frozen in general or in the current viewport.";}
		: toggle {
			key = "DoCurrentTabOnly";
			label = "Limit search to bubble notes in paper space of the current layout only";
		}
		: toggle {
			key = "BubbleHooks";
			label = "Use landing (hook) on bubble notes";
		}
		: edit_box {
			key = "NoteTypes";
			label = "Note shapes";
		}
		: row {
			: popup_list {
				key = "InsertTablePhases";
				label = "Number of phases to set for all drawings in project (or \"No\" to leave drawings alone)";
			}
		}
		: boxed_row {
			label = "Phase alias names";
			: edit_box {
				key = "PhaseAlias1";
				label = "1";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias2";
				label = "2";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias3";
				label = "3";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias4";
				label = "4";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias5";
				label = "5";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias6";
				label = "6";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias7";
				label = "7";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias8";
				label = "8";
				fixed_width = true;
				edit_width = 2;
			}
			: edit_box {
				key = "PhaseAlias9";
				label = "9";
				fixed_width = true;
				edit_width = 2;
			}
		}
	}
	close_button;
}

HCNMKey : dialog {
	key = "Title";
	label = "";
	: boxed_column {
		label = "Show";
		: toggle {
			key = "ShowKeyTableQuantities";
			label = "Show quantities in Key Notes Tables";
		}
		: toggle {
			key = "ShowKeyTableGrid";
			label = "Show grid lines in Key Notes Tables";
		}
		: toggle {
			key = "ShowKeyTableTitleShapes";
			label = "Show title shape blocks in Key Notes Tables";
		}
	}
	: boxed_column {
		label = "Lines";
		: edit_box {
			key = "DescriptionWrap";
			label = "Description wrap line length (characters)";
		}
		: edit_box {
			key = "LineSpacing";
			label = "Line spacing/height (text heights)";
		}
		: edit_box {
			key = "NoteSpacing";
			label = "Spacing (text heights) around each note or group of titles";
		}
	}
	: boxed_column {
		label = "Columns";
		: edit_box {
			key = "TableWidth";
			label = "Horizontal spacing (text heights) of a split key notes table. (Adjust to match one-phase NOTEQTY block width.)";
				fixed_width = true;
				edit_width = 3;
		}
		: edit_box {
			key = "PhaseWidthAdd";
			label = "Additional spacing (text heights) for each additional phase. (Adjust to reflect NOTEQTY? blocks.)";
		}
	}
	close_button;
}

HCNMQT : dialog {
	key = "Title";
	label = "";
	: row {
		: column {
			: edit_box {
				key = "NumberToDescriptionWidth";
				label = "Distance (text heights) from center of number to left point of description";
			}
			: edit_box {
				key = "DescriptionToQuantityWidth";
				label = "Distance (text heights) from left point of description to right point of quantity";
			}
			: edit_box {
				key = "QuantityToQuantityWidth";
				label = "Distance (text heights) from one quantity to the next";
			}
			: edit_box {
				key = "QuantityToUnitsWidth";
				label = "Distance (text heights) from right point of quantity to left point of units";
			}
		}
	}
	close_button;
}

HCNMTable : dialog {
	key = "Title";
	label = "";
	close_button;
}

HCNMLdrblkOptions : dialog {
	key = "Title";
	label = "";
	: boxed_column {
		label = "Show";
		: toggle {
			key = "ShowKeyTableQuantities";
			label = "Show quantities in Key Notes Tables";
		}
		: toggle {
			key = "ShowKeyTableGrid";
			label = "Show grid lines in Key Notes Tables";
		}
		: toggle {
			key = "ShowKeyTableTitleShapes";
			label = "Show title shape blocks in Key Notes Tables";
		}
	}
	: boxed_column {
		label = "Lines";
		: edit_box {
			key = "DescriptionWrap";
			label = "Description wrap line length (characters)";
		}
		: edit_box {
			key = "LineSpacing";
			label = "Line spacing/height (text heights)";
		}
		: edit_box {
			key = "NoteSpacing";
			label = "Spacing (text heights) around each note or group of titles";
		}
	}
	: boxed_column {
		label = "Columns";
		: edit_box {
			key = "TableWidth";
			label = "Horizontal spacing (text heights) of a split key notes table. (Adjust to match one-phase NOTEQTY block width.)";
				fixed_width = true;
				edit_width = 3;
		}
		: edit_box {
			key = "PhaseWidthAdd";
			label = "Additional spacing (text heights) for each additional phase. (Adjust to reflect NOTEQTY? blocks.)";
		}
	}
	ok_cancel;
}

