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
	: button {label = "&Bubble Notes..."; key = "Bubble";}
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
	: text {label = "CNM never searches layers frozen in general or in the current viewport.";}
	: toggle {
		key = "DoCurrentTabOnly";
		label = "Limit search to bubble notes in paper space of the current layout only";
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
	close_button;
}

HCNMBubble : dialog {
	key = "Title";
	label = "";
		: toggle {
			key = "BubbleHooks";
			label = "Use landing (hook) on bubble notes";
		}
		: toggle {
			key = "BubbleMtext";
			label = "Use multiline text in bubble notes. (Enables mtext background masking, but may cause other problems.)";
		}
		: toggle {
			key = "BubbleAreaIntegral";
			label = "Change to Integral arrowhead when adding area quantities.";
		}
		: edit_box {
			key = "NoteTypes";
			label = "Note shapes";
		}
	: boxed_column {
		label = "Interface";
		: boxed_row {
			label = "Text lines to prompt for (User setting)";
			: toggle {
				key = "BubbleTextLine1PromptP";
				label = "Line 1";
			}
			: toggle {
				key = "BubbleTextLine2PromptP";
				label = "Line 2";
			}
			: toggle {
				key = "BubbleTextLine3PromptP";
				label = "Line 3";
			}
			: toggle {
				key = "BubbleTextLine4PromptP";
				label = "Line 4";
			}
			: toggle {
				key = "BubbleTextLine5PromptP";
				label = "Line 5";
			}
			: toggle {
				key = "BubbleTextLine6PromptP";
				label = "Line 6";
			}
			: toggle {
				key = "BubbleTextLine0PromptP";
				label = "Line 0";
			}
		}
		: toggle {
			key = "BubbleSkipEntryPrompt";
			label = "Automatic text by default. Skip initial prompt for text entry. (User setting)";
		}
	}
	: boxed_column {
		label = "Formatting";
		: toggle {
			key = "BubbleOffsetDropSign";
			label = "Drop offset sign";
		}
		: row {
			: column {
				: text {label = "Option ->";}
				: text {label = "Quantity: Linear Feet";}
				: text {label = "Quantity: Square Feet";}
				: text {label = "Quantity: Square Yards";}
				: text {label = "Location: Station";}
				: text {label = "Location: Offset positive";}
				: text {label = "Location: Offset negative";}
				: text {label = "Location: Northing";}
				: text {label = "Location: Easting";}
				: text {label = "Elevation";}
				: text {label = "Pipe: Diameter";}
				: text {label = "Pipe: Slope";}
				: text {label = "Pipe: Length";}
				: text {label = "Where saved ->";}
			}
		: column {
				: text {label = "Prefix";}
				: edit_box {
					key = "BubbleTextPrefixLF";
				}
				: edit_box {
					key = "BubbleTextPrefixSF";
				}
				: edit_box {
					key = "BubbleTextPrefixSY";
				}
				: edit_box {
					key = "BubbleTextPrefixSta";
				}
				: edit_box {
					key = "BubbleTextPrefixOff+";
				}
				: edit_box {
					key = "BubbleTextPrefixOff-";
				}
				: edit_box {
					key = "BubbleTextPrefixN";
				}
				: edit_box {
					key = "BubbleTextPrefixE";
				}
				: edit_box {
					key = "BubbleTextPrefixZ";
				}
				: edit_box {
					key = "BubbleTextPrefixPipeDia";
				}
				: edit_box {
					key = "BubbleTextPrefixPipeSlope";
				}
				: edit_box {
					key = "BubbleTextPrefixPipeLength";
				}
				: text {label = "Project";}
			}
			: column {
				: text {label = "Postfix";}
				: edit_box {
					key = "BubbleTextPostfixLF";
				}
				: edit_box {
					key = "BubbleTextPostfixSF";
				}
				: edit_box {
					key = "BubbleTextPostfixSY";
				}
				: edit_box {
					key = "BubbleTextPostfixSta";
				}
				: edit_box {
					key = "BubbleTextPostfixOff+";
				}
				: edit_box {
					key = "BubbleTextPostfixOff-";
				}
				: edit_box {
					key = "BubbleTextPostfixN";
				}
				: edit_box {
					key = "BubbleTextPostfixE";
				}
				: edit_box {
					key = "BubbleTextPostfixZ";
				}
				: edit_box {
					key = "BubbleTextPostfixPipeDia";
				}
				: edit_box {
					key = "BubbleTextPostfixPipeSlope";
				}
				: edit_box {
					key = "BubbleTextPostfixPipeLength";
				}
				: text {label = "Project";}
			}
			: column {
				: text {label = "Join w/ next using";}
				: edit_box {				
					key = "BubbleTextJoinDelLF";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelSF";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelSY";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelSta";
				}
				: edit_box {
					key = "BubbleTextJoinDelOff+";
					is_enabled = false;
				}
				: edit_box {
					key = "BubbleTextJoinDelOff-";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelN";
				}
				: edit_box {				

					key = "BubbleTextJoinDelE";
					is_enabled = false;
				}
				: edit_box {				

					key = "BubbleTextJoinDelZ";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelPipeDia";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelPipeSlope";
					is_enabled = false;
				}
				: edit_box {				
					key = "BubbleTextJoinDelPipeLength";
					is_enabled = false;
				}
				: text {label = "Project";}
			}
			: column {
				: text {label = "Precision";}
				: edit_box {
					key = "BubbleTextPrecisionLF";
				}
				: edit_box {
					key = "BubbleTextPrecisionSF";
				}
				: edit_box {
					key = "BubbleTextPrecisionSY";
				}
				: edit_box {
					key = "BubbleTextPrecisionSta";
					is_enabled = false;
				}
				: edit_box {
					key = "BubbleTextPrecisionOff+";
				}
				: edit_box {
					key = "BubbleTextPrecisionOff-";
					is_enabled = false;
				}
				: edit_box {
					key = "BubbleTextPrecisionN";
				}
				: edit_box {
					key = "BubbleTextPrecisionE";
				}
				: edit_box {
					key = "BubbleTextPrecisionZ";
				}
				: edit_box {
					key = "BubbleTextPrecisionPipeDia";
				}
				: edit_box {
					key = "BubbleTextPrecisionPipeSlope";
				}
				: edit_box {
					key = "BubbleTextPrecisionPipeLength";
				}
				: text {label = "User";}
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
// HCNMEditBubble dialog
HCNMEditBubble : dialog {
	key = "Title";
	label = "";
	: row {
		: column {
			: edit_box {
				key = "PrefixNOTENUM";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTEPHASE";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTEGAP";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT1";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT2";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT3";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT4";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT5";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT6";
				label = "";
			}
			: edit_box {
				key = "PrefixNOTETXT0";
				label = "";
			}
		}
		: column {	
			: edit_box {
				key = "EditNOTENUM";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTEPHASE";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTEGAP";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT1";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT2";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT3";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT4";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT5";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT6";
				label = "";
				is_enabled = false;
			}
			: edit_box {
				key = "EditNOTETXT0";
				label = "";
				is_enabled = false;
			}
		}
		: column {
			: edit_box {
				key = "PostfixNOTENUM";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTEPHASE";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTEGAP";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT1";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT2";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT3";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT4";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT5";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT6";
				label = "";
			}
			: edit_box {
				key = "PostfixNOTETXT0";
				label = "";
			}
		}
		: radio_column {
			key = "NoteTextRadioColumn";
			fixed_width = true;
			: radio_button {
				key = "RadioNOTENUM";
				label = "Note number";
				is_enabled = false;
 			}
			: radio_button {
				key = "RadioNOTEPHASE";
				label = "Note phase";
				is_enabled = false;
 			}
			: radio_button {
				key = "RadioNOTEGAP";
				label = "Gap text";
				is_enabled = false;
			}
			: radio_button {
				key = "RadioNOTETXT1";
				label = "Line 1 text";
			}
			: radio_button {
				key = "RadioNOTETXT2";
				label = "Line 2 text";
			}
			: radio_button {
				key = "RadioNOTETXT3";
				label = "Line 3 text";
			}
			: radio_button {
				key = "RadioNOTETXT4";
				label = "Line 4 text";
			}
			: radio_button {
				key = "RadioNOTETXT5";
				label = "Line 5 text";
			}
			: radio_button {
				key = "RadioNOTETXT6";
				label = "Line 6 text";
			}
			: radio_button {
				key = "RadioNOTETXT0";
				label = "Line 0 text";
			}
		}
	}
	: row {
		: button {label = "LF"; key = "LF";}
		: button {label = "SF"; key = "SF";}
		: button {label = "SY"; key = "SY";}
		: button {label = "N"; key = "N";}
		: button {label = "E"; key = "E";}
		: button {label = "NE"; key = "NE";}
		: button {label = "Z"; key = "Z";}
	}
	: row {
		: button {label = "Sta"; key = "Sta";}
		: button {label = "Off"; key = "Off";}
		: button {label = "StaOff"; key = "StaOff";}
		: button {label = "Name"; key = "AlName";}
		: button {label = "StaName"; key = "StaName";}
	}
	: row {
		: button {label = "Dia"; key = "Dia";}
		: button {label = "Slope"; key = "Slope";}
		: button {label = "L"; key = "L";}
		: button {label = "_"; key = "ClearAuto"; width = 3;}
	}
	ok_cancel;	
}
