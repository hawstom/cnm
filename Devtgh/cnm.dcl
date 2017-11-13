// CNM.DCL
//
//
// CNMOptions dialog
CNMOptions : dialog {
	key = "Title";
	label = "";
	: boxed_column {
		label = "User";
		: row {
			: popup_list {
				key = "LayersEditor";
			}
			: text {
				key = "LayersEditorPrompt";
				label = "Layer Settings editor program";
			}
		}
	}
	: boxed_column {
		label = "Project";
		: text {
			key = "ProjectFolder";
			label = "";
		}
		: boxed_column {
			label = "General";
			: row {
			: column {
				: edit_box {
					key = "ProjectNotes";
				}
				: popup_list {
					key = "ProjectNotesEditor";
				}
				: edit_box {
					key = "NoteTypes";
				}
			}
			: column {
			fixed_height = true;
			alignment = top;
				: button {
					key = "ProjectNotesBrowse";
					label = "Browse...";
				}
			}
			: column {
				: text {
					key = "ProjectFolderPrompt";
					label = "Project Notes file";
				}
				: text {
					key = "ProjectNotesEditorPrompt";
					label = "Project Notes editor program";
				}
				: text {
					key = "NoteTypesPrompt";
					label = "Note types (shapes)";
				}
			}
				:spacer {
				}
			}
			: row {
				: popup_list {
					key = "InsertTablePhases";
				}
				: text {
					key = "InsertTablePhasesPrompt";
					label = "Number of phases to set for all drawings in project (or \"No\" to leave drawings alone)";
				}
			}
			: boxed_row {
				label = "Phase alias names";
				: column {
					: edit_box {
						key = "PhaseAlias1";
					}
					: edit_box {
						key = "PhaseAlias2";
					}
					: edit_box {
						key = "PhaseAlias3";
					}
				}
				: column {
					: text {
						key = "PhaseAlias1Prompt";
						label = "1";
					}
					: text {
						key = "PhaseAlias2Prompt";
						label = "2";
					}
					: text {
						key = "PhaseAlias3Prompt";
						label = "3";
					}
				}
				: column {
					: edit_box {
						key = "PhaseAlias4";
					}
					: edit_box {
						key = "PhaseAlias5";
					}
					: edit_box {
						key = "PhaseAlias6";
					}
				}
				: column {
					: text {
						key = "PhaseAlias4Prompt";
						label = "4";
					}
					: text {
						key = "PhaseAlias5Prompt";
						label = "5";
					}
					: text {
						key = "PhaseAlias6Prompt";
						label = "6";
					}
				}
				: column {
					: edit_box {
						key = "PhaseAlias7";
					}
					: edit_box {
						key = "PhaseAlias8";
					}
					: edit_box {
						key = "PhaseAlias9";
					}
				}
				: column {
					: text {
						key = "PhaseAlias7Prompt";
						label = "7";
					}
					: text {
						key = "PhaseAlias8Prompt";
						label = "8";
					}
					: text {
						key = "PhaseAlias9Prompt";
						label = "9";
					}
				}
			}
		}
		: boxed_column {
			label = "Bubble Notes";
			: toggle {
				key = "BubbleHooks";
				label = "Use landing (hook) on bubble notes";
			}
			: toggle {
				key = "DoCurrentTabOnly";
				label = "Limit bubble note search to current tab";
			}
		}
		: boxed_column {
			label = "Tables";
			: row {
					: edit_box {
						key = "DescriptionWrap";
					}
					: text {
						key = "WrapPrompt";
						label = "Description wrap line length (characters)";
					}
					: edit_box {
						key = "LineSpacing";
					}
					: text {
						key = "LineSpacingPrompt";
						label = "Distance (text heights) between lines";
					}
					: edit_box {
						key = "NoteSpacing";
					}
					: text {
						key = "NoteSpacingPrompt";
						label = "Distance (text heights) between notes";
					}
			}
			: boxed_column {
				label = "Key Notes Table";
			: toggle {
				key = "ShowKeyTableQuantities";
				label = "Show quantities in Key Notes Tables";
			}
			: toggle {
				key = "ShowKeyTableGrid";
				label = "Show grid lines in Key Notes Tables";
			}
			: row {
				: column {
						: edit_box {
							key = "TableWidth";
						}
						: edit_box {
							key = "PhaseWidthAdd";
						}
					}
					: column {
						: text {
							key = "TableWidthPrompt";
							label = "Horizontal spacing (text heights) of a split key notes table. (Adjust to match one-phase NOTEQTY block width.)";
						}
						: text {
							key = "PhaseWidthAddPrompt";
							label = "Additional spacing (text heights) for each additional phase. (Adjust to reflect NOTEQTY? blocks.)";
						}
					}
				}
			}
			: boxed_column {
			label = "Quantity Take-off Table";
				: row {
					: column {
						: edit_box {
							key = "NumberToDescriptionWidth";
						}
						: edit_box {
							key = "DescriptionToQuantityWidth";
						}
						: edit_box {
							key = "QuantityToQuantityWidth";
						}
						: edit_box {
							key = "QuantityToUnitsWidth";
						}
					}
					: column {
						: text {
							key = "NumberToDescriptionWidthPrompt";
							label = "Distance (text heights) from center of number to left point of description";
						}
						: text {
							key = "DescriptionToQuantityWidthPrompt";
							label = "Distance (text heights) from left point of description to right point of quantity";
						}
						: text {
							key = "QuantityToQuantityWidthPrompt";
							label = "Distance (text heights) from one quantity to the next";
						}
						: text {
							key = "QuantityToUnitsWidthPrompt";
							label = "Distance (text heights) from right point of quantity to left point of units";
						}
					}
				}
			}
		}
	}  
  ok_cancel;
}
/*
ProjectNotes=constnot.txt
ProjectNotesEditor=notepad.exe
NoteTypes=BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI
DoCurrentTabOnly=0
PhaseAlias1=1
PhaseAlias2=2
PhaseAlias3=3
PhaseAlias4=4
PhaseAlias5=5
PhaseAlias6=6
PhaseAlias7=7
PhaseAlias8=8
PhaseAlias9=9
InsertTablePhases=No
TableWidth=65
PhaseWidthAdd=9
LineSpacing=3
NoteSpacing=3
NumberToDescriptionWidth=2.5
DescriptionToQuantityWidth=56
QuantityToQuantityWidth=9
QuantityToUnitsWidth=1
ShowKeyTableGrid=0
ShowKeyTableQuantities=0
BubbleHooks=No
BubbleLeaderConnectOsnap=mid,end
ImportLayerSettings=No
*/