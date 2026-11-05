# Methodology and Specifications – Nature Opportunity Assessment (v4)

Source ID in the workbook: **SRC-METH**. Upload this file to the Copilot agent's Knowledge as .docx or .pdf. It matches the tabs of **Nature_Opportunity_Master_Template.xlsx** (v4) and the deck skeleton **Nature_Deck_Skeleton_EY.pptx** (cover + 4 pages).

---

## 1. Purpose and scope

The Nature Opportunity Assessment shows, for one client in one country, where national nature spending is heading, how that meets the client's business, and what the client already does.

- **Prepared by:** EY CCaSS (Climate Change and Sustainability Services).
- **Inputs:**
  - the client's public documents
  - the EY NBSAP Nature Sizing workbook (SRC-EY), where NBSAP = National Biodiversity Strategy and Action Plan
- **Output:** a 5-page slide pack (cover + 4 pages) on the EY template:
  - pages 2–4 are produced by this method
  - page 5 (How EY can support) is written by the EY team, not the agent

---

## 2. Principles

1. **Evidence only.** Use only the uploaded sources. No general knowledge about the client.
2. **Verbatim.** Every quote or figure is copied exactly, at 25 words or fewer, with:
   - the printed page (or tab)
   - the PDF page
3. **EY view.** Any interpretation or judgement is labelled "EY view". It must cite the evidence it rests on and give a one-line rationale.
4. **Absence checks.** A statement that something is *not* disclosed ("not yet visible") must:
   - list the sources searched
   - be phrased neutrally: "not found in the documents reviewed"
5. **Traceability.** Nothing appears on a slide without a trace (section 7).
6. **Numbers from formulas.** Totals, shares, percentages and scores are calculated in the workbook. The agent writes words; where the workbook calculates a number, the agent never retypes it from memory.
7. **Tone.** UK English, neutral, factual. Never criticise the client. Describe gaps as "not yet visible" or "not disclosed".

---

## 3. Taxonomy

The taxonomy has five themes, 15 sub-themes and about 80 sub-markets, all from the EY workbook's Markets and Sub_Markets tabs.

- **Theme names** are toolkit labels derived from the Market_ID prefixes (to be confirmed with the EY workbook owner).
- **Short names** are used on slides. They are defined in the Glossary (DEF-TH-*).

| Theme (slide short name) | Sub-themes |
|---|---|
| Water (Water) | W01 Water usage · W02 Water quality · W03 Water availability |
| Agriculture, forestry and fisheries (Agriculture and forestry) | AFF01 Sustainable forestry · AFF02 Sustainable agriculture · AFF03 Sustainable fisheries and aquaculture |
| Urban development and infrastructure (Urban development) | UD01 Nature-positive buildings · UD02 Green-blue infrastructure · UD03 Land-use change and habitat protection |
| Disaster management and adaptation (Flood, coastal and adaptation) | DM01 Flood and coastal risk reduction · DM02 Flood mitigation · DM03 Long-term adaptation and resilience |
| Pollution and waste (Pollution and waste) | P01 Sustainable packaging · P02 Waste and circular economy · P03 Air pollution, disturbances and noise |

**Mapping rules**
- Map evidence to the sub-market, which determines the parent sub-theme.
- XC = cross-cutting (group-level targets, total finance figures).

**Classification review**
- An analyst may move an NBSAP commitment to a different sub-theme where the EY tag misrepresents it, using columns N–O of NBSAP_Commitments:
  - **N:** the new sub-theme
  - **O:** the rationale
- The original EY tag is kept.
- The GBR rows reviewed so far:
  - M01 and M09 (storm overflows) → W02 (water quality)
  - M120 and M121 (flood schemes) → DM01 (flood and coastal risk reduction)
- Tell the EY workbook owner about any change.

---

## 4. Scoring criteria (each scored 1–5)

| Code | Criterion | Weight | Who scores |
|---|---|---|---|
| C1 | Materiality to client | 25% | Agent (judgement, with Evidence_IDs) |
| C2 | Action gap | 20% | Agent |
| C3 | NBSAP emphasis | 15% | Workbook: number of the country's commitments in the sub-theme |
| C4 | Regulatory and market momentum | 15% | Agent |
| C5 | Market opportunity | 25% | Workbook: sub-theme share of the country's total commitment value |

**C1 Materiality**
- 5: The client calls the topic material, or it relates to a core business line or large disclosed exposure.
- 4: Clearly linked to a significant activity or exposure.
- 3: Linked to some activities.
- 2: Limited or indirect relevance.
- 1: No relevance in the sources.

**C2 Action gap** (higher = more room to develop)
- 5: Material, but no disclosed action.
- 4: Statements or advocacy only.
- 3: Commitments or eligibility criteria, but no quantified nature target.
- 2: Quantified target with limited progress reporting.
- 1: Quantified nature target with reported progress.
- **Cap:** C2 ≤ 2 when C1 ≤ 2.

**C3 bands (number of commitments)**

| Commitments | Score |
|---|---|
| 4 or more | 5 |
| 3 | 4 |
| 2 | 3 |
| 1 | 2 |
| 0 | 1 |

**C4 Regulatory and market momentum**
- 5: Mandatory requirement in force or confirmed.
- 4: Announced or consulted requirement, or a widely adopted framework.
- 3: Emerging voluntary framework.
- 2: Limited drivers.
- 1: None identified.

**C5 bands (share of the country's total commitment value)**

| Share of total | Score |
|---|---|
| 15% or more | 5 |
| 5% or more | 4 |
| 2% or more | 3 |
| 0.5% or more | 2 |
| Below 0.5% | 1 |

C5 uses shares rather than amounts so that it works in any country and currency. It still reflects size: the more of the national money a sub-theme attracts, the more capital a bank could deploy there.

---

## 5. Weighted score, tiers and confidence

- **Score:** the weighted average of C1–C5, rounded to one decimal place (theme scores to two).
- **Tiers:**
  - High: 4.0 or above
  - Medium: 3.0–3.9
  - Low: below 3.0
- **Tie-break:** higher C1, then higher C2, then more client evidence items.
- **Confidence:**
  - High: 3 or more client items from 2 or more sources, with at least one Direct mapping
  - Medium: at least 1 client item
  - Low: no client evidence

---

## 6. Theme roll-up

Themes are scored so that a topic appearing several times (for example, three water sub-themes) is judged as a whole:

- **Theme C1, C2 and C4:** the highest sub-theme score in the theme.
- **Theme C3 and C5:** recalculated from the theme's total commitments (number and share), using the same bands.
- **Weights:** the same as for sub-themes.
- **Drill-down:** the sub-theme scores stay visible on page 3.

---

## 7. Traceability system

Every value on pages 2–4 is traced. The chain is:

> **Slide text → Claim_ID (Claims_Register) → primary trace ID → source document and page**

| Trace prefix | Resolves to | Example |
|---|---|---|
| EV- | Evidence_Log row: verbatim quote, Source_ID, printed page, PDF page | EV-040 |
| NB- | NBSAP_Commitments row (EY workbook, Market_Examples) | NB-M01 |
| CALC- | Calculations tab: formula, inputs, Source_ID | CALC-52 |
| SC- | Scoring row (sub-theme score) | SC-W02 |
| DEF- / ABB- | Glossary: definition or abbreviation, with Source_ID and reference | ABB-PRA |
| SRC- | Sources tab: the document itself | SRC-02 |

**Claim types**

| Type | Rule |
|---|---|
| Quoted fact | Primary trace = EV- (or NB-). Unit conversions (e.g. £m to £bn) are written in the derivation column. |
| Calculated | Primary trace = CALC-. Excel checks that the figure on the slide matches the formula result. |
| Definition / Abbreviation | Primary trace = DEF- / ABB-. Every abbreviation on a slide needs an ABB- entry. |
| EY view | Primary trace = the main supporting EV-. The rationale must be filled in. |
| Absence check | Primary trace = DEF-NOTVISIBLE or the SRC- searched. The derivation must list every source searched. |

**Rules**
- Claims for text that Excel writes itself (auto tokens) use the key `auto:<token>`.
- Speaker notes are built automatically:
  - first the talk track
  - then `[Sources]`, one line per claim (Claim_ID, trace IDs, source title, page)
  - then `[/Sources]`
- **Page numbering:**
  - The printed page is the number shown on the page.
  - The PDF page is the page number in the PDF viewer.
  - Sources records how the two relate for each document (e.g. "printed = PDF − 1"). If a document has no printed numbers, use "PDF p.N".

---

## 8. Output tables (exact headers, in order)

**Sources:** Source_ID | Title | Publisher | Year | Type | File name / URL | Date accessed | Page numbering | Included (Y/N) | Note

**Client_Exposure:** Evidence_ID | Source_ID | Sector (as disclosed) | Sub-sector | Exposure (m, reporting currency) | Currency | Nature priority (as disclosed) | Row type | Linked themes (EY view) | Reference (tab/page)
- Row type: Sector total, Sub-sector, Subtotal or Group total.
- Every exposure row is also an Evidence_Log row with the same Evidence_ID.

**Evidence_Log:** Evidence_ID | Source_ID | Printed page / tab | PDF page | Verbatim quote or figure (≤25 words, exact) | Evidence type | Scope | Summary | Verified against source (initials, date)
- Evidence type: Commitment, Quantified target, Policy, Exposure, Financing or product, Partnership, Performance metric, Disclosure, Governance, External driver.
- Scope: Own operations, Value chain / portfolio, Products and services, External driver.

**NBSAP_Mapping:** Evidence_ID | Source_ID | Evidence class | Primary SubTheme_ID | Primary Sub_Market_ID | Secondary SubTheme_ID | Mapping strength | Rationale
- Evidence class: Client exposure, Client action, Client position or External driver.
- Mapping strength: Direct or Indirect.

**Scoring (agent part):** SubTheme_ID | C1 | C2 | C4 | Evidence_IDs | Rationale | Client exposure (as disclosed)

**Classification review (optional):** Example_ID | Current SubTheme_ID | Proposed SubTheme_ID | Rationale (EY view)

**Glossary (client entries):** ID | Term | Type | Definition / expansion | Source_ID | Reference (page/tab)

**Calculations (client entries, from row 30):** CALC_ID | Description | Formula (Excel, referencing Client_Exposure cells) | Unit | How it is calculated | Input trace IDs | Source_ID

**Slide_Content:** Key | Value | Claim_IDs (the keys are listed in section 10)

**Claims_Register:** Claim_ID | Slide | Slide_Content key | Claim (exact wording or value on the slide) | Claim type | Primary trace ID | Other trace IDs | Derivation / how checked

---

## 9. Standard terms and style rules

**Notation**
- £bn = billion pounds sterling; £m = million pounds sterling (use the country's currency symbol).
- ha = hectares.
- c. = approximately (rounded from the source figure).
- Write money as £12.1bn or £120m, and percentages as 9.3%.
- Billions take one decimal place where the source allows.

**Standard abbreviations** (each needs an ABB- entry when used)
- EY (Ernst & Young)
- NBSAP (National Biodiversity Strategy and Action Plan)
- KPI (key performance indicator)
- ENCORE (Exploring Natural Capital Opportunities, Risks and Exposure)

**Wording**
- Spell out any other abbreviation in the Glossary, with its source.
- Use action titles of 68 characters or fewer, stating the finding, not the topic.
- "Not yet visible" means not found in the documents reviewed. Always say which documents.

---

## 10. Slide specification (cover + 4 pages)

| Page | Purpose | Agent writes (Slide_Content keys) | Excel writes (Deck_Tokens) |
|---|---|---|---|
| Cover | Title and date | cover_title, cover_date, client_name, client_short | – |
| 2 Context | Where national nature money goes and where the country is heading | s1_title, s1_section, s1_callout_text, s1_heading_r, s1_sub_r, s1_tile1–4_stat/text, s1_rules_lead, s1_rules, s1_sowhat, s1_footnote, s1_talk | s1_left_head, s1_left_sub, s1_callout_stat, Chart A, s1_notes |
| 3 Context × client | Theme priority, national money and the client's main link, row by row | s2_title, s2_section, s2_link_W, s2_link_AFF, s2_link_UD, s2_link_DM, s2_link_P, s2_sowhat, s2_talk | r1–r5 theme, drill-down, money and link (sorted by score), s2_footnote (method), Charts B and C, s2_notes |
| 4 Client | For the top three themes by score: what the client does today, what is not yet visible, and the opportunity | s3_title, s3_section, s3_col1–3_today/gap/opp, s3_sowhat, s3_footnote, s3_talk | h1–h3 (column headers), s3_notes |
| 5 EY | How EY can support | **Nothing: written by the EY team** | – |

**Charts:** paste the Deck_Chart blocks into the charts (Edit Data):
- **Chart A:** page 2, value by theme
- **Chart B:** page 3, theme score
- **Chart C:** page 3, value by theme in score order

---

## 11. QA checklist (all must pass before the deck is filled)

The QA_Log tab runs 19 checks automatically (items 6 and 12 below each cover two):
1. Country recognised.
2. Commitments loaded.
3. All 15 sub-themes scored.
4. Weights total 100%.
5. C2 cap respected.
6. Every evidence item is mapped and has a source, page and quote.
7. Every Source_ID exists.
8. Every exposure row is in Evidence_Log.
9. Mapping IDs are valid.
10. Every classification override has a rationale.
11. Slide text is within limits.
12. **Trace:** every slide value has valid Claim_IDs.
13. **Trace:** every claim resolves.
14. **Trace:** no orphan claims.
15. **Trace:** calculated figures match their formulas.
16. **Trace:** every glossary entry has a source.
17. **Trace:** every abbreviation used is cited.
18. EXAMPLE rows deleted.

The analyst also checks these by hand:
- Spot-check 5 quotes against the PDFs, using the PDF page.
- Read every EY view rationale.
- Confirm the absence checks against any report not reviewed (e.g. the annual report).
- Make sure page 5 has no [EY team] placeholders left.
