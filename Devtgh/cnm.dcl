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
				: edit_box {
					key = "ProjectNotes";
					label = "Project Notes file";
				}
				: button {
					key = "ProjectNotesBrowse";
					label = "Browse...";
				}
			}
			: popup_list {
				key = "ProjectNotesEditor";
				label = "Project Notes editor program";
			}
			: edit_box {
				key = "NoteTypes";
				label = "Note types (shapes)";
			}
			:spacer {
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
				}
				: edit_box {
					key = "PhaseAlias2";
					label = "2";
				}
				: edit_box {
					key = "PhaseAlias3";
					label = "3";
				}
				: edit_box {
					key = "PhaseAlias4";
					label = "4";
				}
				: edit_box {
					key = "PhaseAlias5";
					label = "5";
				}
				: edit_box {
					key = "PhaseAlias6";
					label = "6";
				}
				: edit_box {
					key = "PhaseAlias7";
					label = "7";
				}
				: edit_box {
					key = "PhaseAlias8";
					label = "8";
				}
				: edit_box {
					key = "PhaseAlias9";
					label = "9";
				}
			}
		}
		: boxed_row {
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
						label = "Description wrap line length (characters)";
					}
					: edit_box {
						key = "LineSpacing";
						label = "Distance (text heights) between lines";
					}
					: edit_box {
						key = "NoteSpacing";
						label = "Distance (text heights) between notes";
					}
			}
			: boxed_column {
				label = "Key Notes Table";
			: row {
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
			: row {
				: column {
						: edit_box {
							key = "TableWidth";
							label = "Horizontal spacing (text heights) of a split key notes table. (Adjust to match one-phase NOTEQTY block width.)";
						}
						: edit_box {
							key = "PhaseWidthAdd";
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
			}
		}
	}  
  ok_cancel;
}