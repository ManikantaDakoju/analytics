# Methodology and Specifications: Nature Priorities Analysis

Version 1.0. This document is the reference the agent must follow. Text in [square brackets] is a placeholder for the EY team to complete. Do not leave placeholders unresolved in client outputs.

---

## Section A. Master Workbook specification

If the Master Workbook uses different sheet or column names, update this section to match it exactly before use.

### A1. Inputs
| Field | Description |
|---|---|
| Client | Full legal or trading name and short name, e.g. Lloyds Banking Group (LBG) |
| Country | Geography in scope |
| Sector | Primary sector (see Section E) |
| Audience | Internal or External |
| As-at date | Date the analysis reflects |
| Prepared by | Team name (no individual names in client outputs) |

### A2. Sources
| Column | Description |
|---|---|
| Source_ID | SRC-01, SRC-02, ... |
| Title | Document title |
| Publisher | e.g. client, UK Government (Defra) |
| Year | Publication year |
| Type | Annual report / Sustainability report / Climate or nature disclosure (TCFD, TNFD) / Policy / Website / NBSAP / Other |
| Location | File name or official URL |
| Date accessed | DD/MM/YYYY |
| Included | Y / N |
| Reason if excluded | e.g. older than 24 months, not official |

### A3. Taxonomy (completed by the EY team from the market sizing tool)
| Column | Description |
|---|---|
| Theme_ID | T01, T02, ... |
| Theme | Theme name |
| SubTheme_ID | T01.1, T01.2, ... |
| Sub-theme | Sub-theme name |
| Definition | One-sentence definition used for mapping |
| Market_Opportunity_Score | 1 to 5 from the market sizing tool for this country. Leave blank if not available |

### A4. NBSAP_Targets
| Column | Description |
|---|---|
| Target_ID | As numbered in the NBSAP document |
| Target text | Short wording of the national target or goal |
| Global reference | Linked Kunming-Montreal Global Biodiversity Framework (KMGBF) target, if stated in the NBSAP |
| Lead mechanism | Regulation / Funding / Voluntary / Not stated |

### A5. Evidence_Log
| Column | Description |
|---|---|
| Evidence_ID | EV-001, EV-002, ... |
| Source_ID | From the Sources sheet |
| Page/Section | Exact location |
| Verbatim quote | 25 words or fewer, exact text |
| Evidence type | Commitment / Quantified target / Policy / Exposure / Financing or product / Partnership / Performance metric / Disclosure / Governance |
| Scope | Own operations / Value chain or portfolio / Products and services |
| Summary | One sentence, factual |

### A6. NBSAP_Mapping
| Column | Description |
|---|---|
| Evidence_ID | From Evidence_Log |
| Primary SubTheme_ID | One only |
| Secondary SubTheme_ID | Optional |
| NBSAP Target_ID | Most relevant national target, or "None identified" |
| Mapping strength | Direct (explicitly addresses the topic) / Indirect (related but not explicit) |
| Rationale | One sentence |

### A7. Scoring
| Column | Description |
|---|---|
| Rank | 1 = highest |
| SubTheme_ID | |
| Theme | |
| Sub-theme | |
| C1 Materiality | 1 to 5 |
| C2 Action gap | 1 to 5 |
| C3 NBSAP emphasis | 1 to 5 |
| C4 Regulatory and market momentum | 1 to 5 |
| C5 Market opportunity | 1 to 5, or "n/a" |
| Weighted score | One decimal place |
| Tier | High / Medium / Low |
| Confidence | High / Medium / Low |
| Evidence_IDs | List |
| Rationale | One to two sentences |

### A8. QA_Log
| Column | Description |
|---|---|
| Check | From Section D |
| Result | Pass / Fail |
| Action taken | What was fixed |

---

## Section B. Scoring rubric

[Replace with the EY-approved rubric if one exists. Keep the structure so results remain comparable.]

The rubric identifies where nature topics are most material to the client and where the client has the most room to develop, in the context of national priorities. This is where EY support is most relevant.

### B1. Criteria and weights
| Code | Criterion | Weight | Question answered |
|---|---|---|---|
| C1 | Materiality to client | 30% | How significant is this sub-theme to the client's business model, value chain or portfolio? |
| C2 | Action gap | 25% | How much room is there between what is material and what the client has disclosed or done? |
| C3 | NBSAP emphasis | 20% | How strongly does the national NBSAP prioritise this sub-theme? |
| C4 | Regulatory and market momentum | 15% | How strong are regulatory, disclosure or market drivers, as evidenced in the sources? |
| C5 | Market opportunity | 10% | Score from the market sizing tool (Taxonomy sheet) |

If C5 is blank for every sub-theme, exclude it and rescale the other weights proportionally (C1 33.3%, C2 27.8%, C3 22.2%, C4 16.7%). State this in the output.

### B2. Scoring scales
**C1 Materiality to client**
- 5: The client identifies it as material, or it relates to a core business line or a large share of the portfolio
- 4: Clearly linked to a significant business activity or portfolio segment
- 3: Linked to some activities; moderate relevance
- 2: Limited or indirect relevance
- 1: No identifiable relevance in the sources

The client's own materiality assessment takes precedence. Without one, use disclosed business activities and the sector lens (Section E), and label the result "EY view".

**C2 Action gap** (a higher score means more room to develop)
- 5: Material, but no disclosed action
- 4: General statements only; no commitments
- 3: Commitments or policies, but no quantified targets
- 2: Quantified targets, but limited progress reporting
- 1: Quantified targets with reported progress and governance

If C1 is 1 or 2, cap C2 at 2. An absence of action on an immaterial topic is not a gap.

**C3 NBSAP emphasis**
- 5: Explicit national target with a deadline and a delivery mechanism such as regulation or funding
- 4: Explicit national target without a clear mechanism
- 3: Addressed in national goals or actions, but not as a standalone target
- 2: Mentioned only
- 1: Not addressed in the NBSAP

**C4 Regulatory and market momentum**
- 5: Mandatory requirements in force or confirmed
- 4: Consultation or announced requirements, or widely adopted voluntary frameworks referenced in the sources
- 3: Emerging voluntary frameworks or investor expectations referenced in the sources
- 2: Limited external drivers identified
- 1: None identified

**C5 Market opportunity**
Use the value from the Taxonomy sheet. Do not estimate it.

### B3. Calculations and rules
- Weighted score = sum of (criterion score x weight), to one decimal place.
- Tiers: High is 4.0 or above. Medium is 3.0 to 3.9. Low is below 3.0.
- Tie-break order: higher C1, then higher C2, then more evidence items.
- Confidence:
  - High: 3 or more evidence items from 2 or more sources, with at least one Direct mapping
  - Medium: 1 to 2 evidence items, or only Indirect mappings
  - Low: no client evidence; scores rely on the sector lens or "EY view"
- Every sub-theme in the Taxonomy must be scored, including those with no evidence.

---

## Section C. Slide specification

Maximum 3 slides. Use action titles, which are full-sentence statements of the key message. Do not use topic labels as titles.

### Slide 1. Executive summary
- Suggested layout: Title and content, or two-column with callouts
- [Title]: action title, 15 words or fewer, stating the overall finding
- [Subtitle]: "Mapping [CLIENT]'s public nature disclosures to the [COUNTRY] NBSAP | As at [AS_AT_DATE]"
- [Callouts]: three key figures, such as the number of sub-themes assessed, the number of High-priority sub-themes, and the number of public sources reviewed
- [Body], 120 words or fewer:
  - Context: 1 bullet on why nature matters for this client and the national policy context
  - Key findings: 3 bullets
  - Top priorities: the top 3 sub-themes with their tiers
  - Implication: 1 bullet on the "so what" for the client
- [Footnote]: "Based on publicly available information as at [AS_AT_DATE]. Sources: [Source_IDs]. Scoring methodology on slide 2."

### Slide 2. Nature priorities ranked by score
- Suggested layout: Title and table
- [Title]: action title, 15 words or fewer, summarising the pattern, e.g. where priorities concentrate
- [Table] columns: Rank | Theme | Sub-theme | [COUNTRY] NBSAP target | Score | Tier | Key evidence (12 words or fewer)
- Rows: all High and Medium sub-themes, up to 12 rows. If there are more, show the top 12 and state the remainder in the footnote.
- Order: by rank
- [Footnote]: "Score = weighted average (1 to 5) of materiality 30%, action gap 25%, NBSAP emphasis 20%, regulatory and market momentum 15%, market opportunity 10%. [n] further sub-themes scored Medium or Low; full scoring in the Master Workbook. Sources: [Source_IDs]."
- Optional visual: a 2x2 matrix of Materiality (C1) against Action gap (C2) for the top sub-themes. Describe the placement in text only.

### Slide 3. How EY can assist
- Suggested layout: Title and three or four columns, or Title and table
- [Title]: action title, 15 words or fewer, e.g. "EY can support [CLIENT] to [outcome] across its top nature priorities"
- [Table] columns: Priority area | What the public evidence shows | How EY could support | Indicative output
- Rows: the top 3 or 4 priorities from slide 2
- "How EY could support" must use the EY service list in [EY_SERVICES knowledge file]. If no list is provided, use these generic support types and add "(to be aligned with the EY CCaSS service catalogue)":
  - Nature-related risk and opportunity assessment
  - Strategy, target setting and transition planning
  - Disclosure readiness (e.g. TNFD) and reporting
  - Data, metrics and analytics
  - Sustainable finance product and framework design
  - Governance, policy and capability building
- [Body]: Proposed next steps, 2 to 3 bullets, e.g. a discovery workshop, data review or deep dive on the top priority
- Do not mention fees, guarantees, named individuals or credentials not provided in the knowledge files.

---

## Section D. Quality assurance checklist

1. All sub-themes in the Taxonomy appear in the Scoring sheet.
2. Every evidence item has a Source_ID, page or section, and a verbatim quote.
3. Quotes are exact and 25 words or fewer.
4. No source is excluded without a stated reason. All included sources meet the 24-month and official-source rules.
5. Weights total 100%, or the reweighting is stated.
6. Weighted scores are calculated correctly, and ranks match the scores and tie-break rules.
7. Every C2 score respects the materiality cap.
8. Every figure on the slides matches the workbook.
9. Slide word limits are respected.
10. Acronyms are defined at first use on each slide (NBSAP, KMGBF, TNFD, CCaSS).
11. UK English spelling is used throughout.
12. No confidential information, fees, individual names or unsupported claims appear.
13. Interpretation is labelled "EY view".
14. Low-confidence scores are flagged.

---

## Section E. Sector lens

Use only to support C1 when the client has no materiality assessment. Label the result "EY view".

| Sector | Where nature impacts and dependencies typically sit |
|---|---|
| Banking and financial services | Mainly financed activities: agricultural and land-based lending, commercial real estate and mortgages (land use, development, flood risk), lending to high-impact sectors (food, water, extractives, construction, infrastructure), sustainable finance products, investment and insurance portfolios. Own operations are usually less material. |
| [Other sector] | [To be completed when the agent is reused for a new sector] |

---

## Section F. Tone and style guide

- UK English: organisation, prioritise, programme, analyse.
- Neutral and factual. Use "The disclosures indicate...", "No evidence was identified in public sources...", "EY could support...".
- Avoid "LBG fails to...", "LBG does not...", "cutting-edge", "game-changing", "unlock", "leverage" (as a verb), "world-class", "robust" (unless quoted), and exclamation marks.
- Use sentence case for titles and headings.
- Show scores to one decimal place. Use numerals for all figures on slides.
- Write the client's full name at first use, then the short name.
- Keep to one idea per bullet and 20 words or fewer per bullet.

---

## Section G. Reusing the agent for a new client

1. Update the Inputs: client, country, sector, audience and as-at date.
2. Upload the client's public sources and the country's NBSAP.
3. Load the country's NBSAP targets into the NBSAP_Targets sheet.
4. Confirm the Taxonomy and Market_Opportunity_Score for that country from the market sizing tool.
5. If the sector is new, add a row to the sector lens in Section E.
6. Do not change the rubric, sheet structure or slide specification without team agreement. This keeps outputs comparable across clients.
