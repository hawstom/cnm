2025-10-27
On Saturday we got deep into the woods about style. Now it's time to go deep about meaning (moving beyond style to toward meaning; doesn't that sound like nice progress?).

A. Names, phase 2; moving beyond style to toward meaning: 
1. I am getting lost trying to arrange our functions into groups because I don't know what they all do. I think we are both (all) getting lost because we are still evolving in our understanding of how to rigorously describe this architecture, and amid this evolution (only a week!) we have not yet arrived at good names that would help us clarify our thinking and avoid chaos. This situation is applicable to all our working documents, projects, and languages outside of CNM too; in other words, this will always and everywhere be the case. Excellence and beauty arises when we stop to think more clearly. CNM will be around for many more years, probably in this form. This is unlikely to be throw-away architecture.
2. We had to learn how to "say" the names before we could give our attention to choosing names. And now we know to say them like this: obj-my-beautiful-elegant-vla-thingie, so we can move onward.

B. Core data in working memory: 
1. attribute-list: I like the name "attribute-list" because we are not getting it confused with anything else, partly because it is ubiquitous and inescapable if we are going to work with bubble notes code. But we can change it if it helps us with item b. It's precise and intuitive. In another universe the bubble notes text might live in some other medium. But after 25 years, users and I are on the whole happy with attributed blocks. So "attribute-list" is great and satisfactory because attributed blocks are the venerable architecture of CNM since its beginning. Other possibilities:  lst-attrib or lst-attr.
2. bubble-data: I also like "bubble_data" because it is ubiquitous and inescapable in a smaller scope (the bubble insertion UX). And it contains "attribute-list" plus other tidbits we need. And we define it programmatically. No problem. Other possibilities:  lst-bubble or lst.

C. Core data interfaces or forms (these all need CRUD, and CRUD code for each interface should be grouped in file or a #region)
1. attributes: AutoCAD drawing entity/object. This possibly could also be called dwg, _plan_, block, insertion. I think I like dwg (though it's ambiguous with XDATA), plan (very real-world and unambiguous for now), or (to be modern and freelance-programmer-like, in other words, like ai) acaddoc (the document space).
2. XDATA: I know of no reason why this can't or should be a replica of parts of our working memory since it's invisible to the user. And I think that XDATA is a great name for it. The only other possibility I can think of is bubble-XDATA since that's the specific object that has this XDATA.
3. "dialog" fields: (are they close enough to attribute-list to be considered equivalent? Is it in the interest of our sanity to ensure that they are? Is that possible? I say no.) These differ from attribute-list in that they are free-form, and user can remove underlines that we have to check for and replace when we read from dialog.
4. Reactors: I guess just hcnm-reactor?

D. Core challenges and situations:
1. underlining and overlining: 
  - this is a simple "artistic" business "need" or "imperative" that has given us programmers fun (or fits, as the case may be) for days. Users want to see a line between line1 and line2. It is efficient for the line to be automatically responsive to the longer text length of line1 or line 2. So the founder's early design decision was to use text underlining and overlining. Since then people have experimented with a document line instead. And we programmers could conceivably keep an AutoCAD line reactively the right length (and present or not). But for now, underlining and overlining is our reality handed down from on high. 
  - It would be good to have a more pithy and descriptive name for this than "formatting" (not descriptive; gets confused with concatenation and parsing) and "underlining and overlining" (not pithy). I might like "underover" (precise and cute) or "noteline" (future-proof and functionally descriptive of the line that visually connects the attributes with the bubble) as a multi-purpose verb and noun for this situation and challenge.
2. auto text in free-form attributes:
  - This is not programmatically ideal. And we might think of a better architecture. Or we might not. The great strength of this architecture is that users are artistically free, which they demand to be. I think I am not ashamed of our XDATA and split fields solution. We just need to get better names and definitions for it.
 - When we get input from a user, we don't need to underover/format/noteline it, because we don't need that.
  - When we send text to XDATA, we don't need to underover/format/noteline it, because we don't need that.
  - When we send text to dialog, we do need to underover/format/noteline it, because user expects that as what the attribute contains. If we were building this app from scratch, we probably would say no. But I am reasonably strong in saying that for now, we must format.
3. We are sending data to and from multiple places. 
1. With this reality, disambiguation using verbs is hopeless. If that's the case, we should use either standard verbs like set/get and read/write, and not save, open, update, etc. or no verbs like rtos (real-to-string), xdata-to-dwg, attribute-list-to-dialog, dialog-to-attribute-list, etc.
2. All setters/writers must handle create and update agnostically to the caller. I just want it to be there. I don't care whether it was there before. If I do, I will get/read first.

E. SUMMARY: PROPOSAL FOR IMPROVING NAMES
1. Concepts
a. preferred symbols **highlighted**
b. I am preferring shorter names because 
  - they are notorious, which allows a little more cryptic naming. This does NOT mean that we allow or like short names for all our locals
  - we build function names from these names
c. verbless functions
1. Symbols
i. attribute-list or lst-attrib or lst-attr or **lattribs**
ii. bubble-data = bubble-data or lst-bubble or **lst-bub**
iii. XDATA = **xdata**
iv. reactor = **reactor**
v. underover/format/noteline = **underover** (verb and noun)
vi. block/insertion/plan/document = **dwg**
vii.  reactor = **reactor**

3. Functions
- hcnm-ldrblk-lattribs-to-dwg and dwg-to-lattribs
- hcnm-ldrblk-lattribs-to-dlg and dlg-to-lattribs
- hcnm-ldrblk-lattribs-to-xdata and xdata-to-lattribs
- hcnm-ldrblk-lattribs-to-reactor and reactor-to-lattribs
- hcnm-ldrblk-avport-to-config and config-to-avport ???

Thoughts about this?