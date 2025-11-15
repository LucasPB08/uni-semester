# Strategy Course - Study Workflow

## Purpose
Extract key concepts from textbook screenshots and generate both study notes and Quizlet flashcards for exam preparation.

## Workflow

**Each week:**
1. User provides screenshots of assigned chapters/sections
2. Agent extracts content → creates structured notes in `week-X/notes.md`
3. Agent generates Quizlet-ready flashcards → `week-X/flashcards.md`
4. User copies flashcards to Quizlet for spaced repetition
5. Check off completed week below

## Note Structure

For each chapter/section, organize as:
- **Key Definitions** - Core terms to memorize
- **Frameworks/Models** - Strategic analysis tools with components (Porter's 5 Forces, SWOT, VRIO, etc.)
- **Key Concepts** - Main ideas explained concisely
- **Examples** - Real companies/cases mentioned (useful for flashcard context)

## Flashcard Format

Simple Q&A pairs ready to copy into Quizlet:
```
Q: What are Porter's Five Forces?
A: 1) Threat of new entrants, 2) Bargaining power of suppliers, 3) Bargaining power of buyers, 4) Threat of substitutes, 5) Rivalry among existing competitors

Q: What is strategic positioning?
A: [Definition extracted from text]
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