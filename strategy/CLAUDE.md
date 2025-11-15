# Strategy Course - Study Workflow

## Purpose
Extract key concepts from textbook screenshots and generate both study notes and Quizlet flashcards for exam preparation.

## Workflow

**Each week:**
1. User provides screenshots of assigned chapters/sections
2. Agent extracts content → creates structured notes in `week-X/notes.md`
3. **User reviews notes and provides feedback/approval**
4. **ONLY AFTER notes are finalized:** Agent generates Quizlet-ready flashcards → `week-X/flashcards.md`
5. User copies flashcards to Quizlet for spaced repetition
6. Check off completed week below

## Note Structure

For each chapter/section, organize as:
- **Key Definitions** - Core terms to memorize
- **Frameworks/Models** - Strategic analysis tools with components (Porter's 5 Forces, SWOT, VRIO, etc.)
- **Key Concepts** - Main ideas explained concisely
- **Examples** - Real companies/cases mentioned (useful for flashcard context)

## Flashcard Guidelines

**IMPORTANT - Quantity:**
- Target: **15-20 flashcards per week**
- Variation: ±10 cards (acceptable range: 5-30 cards)
- Quality over quantity - focus on the most important concepts only

**Format - Quizlet Import Ready:**
- Use **tab character** to separate question and answer (for Quizlet import)
- One card per line
- Keep answers concise (1-3 lines maximum)

**Example format:**
```
What are Porter's Five Forces?	1) Threat of new entrants, 2) Bargaining power of suppliers, 3) Bargaining power of buyers, 4) Threat of substitutes, 5) Rivalry among existing competitors
What is strategic positioning?	Deliberately choosing a different set of activities to deliver a unique mix of value
What is competitive advantage?	That which differentiates an organization from its competitors in the eyes of customers
```

## File Organization

```
strategy/
├── CLAUDE.md (this file)
├── Strategy.md (course overview)
├── week-1/
│   ├── notes.md
│   └── flashcards.md
├── week-2/
│   ├── notes.md
│   └── flashcards.md
└── ...
```

## Progress Tracker

**Course Schedule & Completion Status:**

- [ ] **Week 1** (Jan 27-28) - Ch 1, 11.4, 11.5 - What is strategy and intro
- [ ] **Week 2** (Feb 3-4) - Ch 2+3 - External analysis
- [ ] **Week 3** (Feb 10-11) - Ch 4+5+SWOT - Internal analysis
- [ ] **Week 4** (Feb 17-18) - Ch 6,7,8+TOWS - Strategy Formulation I
- [ ] **Week 5** (Feb 24-25) - Ch 5,7,8 - Strategy Formulation II
- [ ] **Week 6** (Mar 3-4) - Ch 10,11 - Strategic Leadership
- [ ] **Week 7** (Mar 10-11) - Ch 9 - Organizational Systems & Change

---

**Note:** Textbook edition may vary (2008 vs 2021), but core strategic concepts remain consistent across editions.