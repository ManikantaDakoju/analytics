# Methodology and Specifications – Nature Opportunity Assessment (v2)

Reference for the Copilot agent. Upload as a knowledge file (save as .docx or .pdf). It matches the tabs of **Nature_Opportunity_Master_Template.xlsx**.

---

## A. Taxonomy (from the EY NBSAP Nature Sizing workbook)

5 themes, 15 sub-themes, about 80 sub-markets. The agent must use these IDs only.

| Theme | SubTheme_IDs |
|---|---|
| Water (W) | W01 Water usage · W02 Water quality · W03 Water availability |
| Agriculture, forestry and fisheries (AFF) | AFF01 Sustainable forestry · AFF02 Sustainable agriculture · AFF03 Sustainable fisheries and aquaculture |
| Urban development and infrastructure (UD) | UD01 Nature-positive buildings · UD02 Green-blue infrastructure, corridors and regeneration · UD03 Land-use change and habitat protection |
| Disaster management and adaptation (DM) | DM01 Risk reduction and prevention · DM02 Flood mitigation · DM03 Long-term adaptation and resilience |
| Pollution and waste (P) | P01 Sustainable packaging and biodegradable materials · P02 Waste and circular economy · P03 Air pollution, disturbances and noise |

Sub-market IDs and definitions are in the Sub_Markets tab. Always map to the sub-market's parent sub-theme. XC = cross-cutting (group-level targets, overall finance totals).

---

## B. Output tables (exact headers, in this order)

**Sources:** Source_ID | Title | Publisher | Year | Type | Location / URL | Date accessed | Included (Y/N) | Reason / note

**Client_Exposure:** Source_ID | Sector (as disclosed) | Sub-sector | Exposure (m, reporting currency) | Currency | Nature priority (as disclosed) | % of total exposure | Linked SubTheme_IDs | Reference (tab/page)

**Evidence_Log:** Evidence_ID | Source_ID | Page / tab | Verbatim quote or figure (≤25 words) | Evidence type | Scope | Summary
- Evidence type: Commitment, Quantified target, Policy, Exposure, Financing or product, Partnership, Performance metric, Disclosure, Governance
- Scope: Own operations, Value chain / portfolio, Products and services, External driver

**NBSAP_Mapping:** Evidence_ID | Source_ID | Evidence class | Primary SubTheme_ID | Primary Sub_Market_ID | Secondary SubTheme_ID | Mapping strength | Rationale
- Evidence class:
  - Client exposure: the client's lending, investment or operations exposure
  - Client action: what the client has done, financed, committed to or made eligible
  - Client position: what the client says or advocates
  - External driver: regulation or market context cited in the documents
- Mapping strength: Direct (explicitly about the sub-theme) or Indirect (related, not explicit)

**NBSAP_Targets:** Country_ID | SubTheme_ID | NBSAP goal / target reference | Policy signal strength (High/Medium/Low) | Page

**Scoring (agent part only):** SubTheme_ID | C1 Materiality | C2 Action gap | C4 Regulatory & market momentum | Evidence_IDs | Rationale | Client exposure (as disclosed) | Key evidence for slide (≤12 words) | Exposure for slide (≤4 words)

**Slide_Content:** Key | Value, using exactly these keys:
- cover_title, cover_date, client_name
- s1_title, s1_section
- s1_stat1_value, s1_stat1_label, s1_stat2_value, s1_stat2_label, s1_stat3_value, s1_stat3_label
- s1_context, s1_finding1, s1_finding2, s1_finding3, s1_priorities, s1_implication, s1_footnote, s1_notes
- s2_title, s2_section, s2_low_note, s2_footnote, s2_notes
- s3_title, s3_section
- s3_card1_title, s3_card1_score, s3_card1_evidence, s3_card1_support, s3_card1_output (and the same five keys for cards 2–4)
- s3_crosscutting, s3_next1, s3_next2, s3_next3, s3_footnote, s3_notes

---

## C. Rubric

| Code | Criterion | Weight | Who scores |
|---|---|---|---|
| C1 | Materiality to client | 30% | Agent |
| C2 | Action gap | 25% | Agent |
| C3 | NBSAP emphasis | 20% | Workbook (number of country NBSAP commitments) |
| C4 | Regulatory and market momentum | 15% | Agent |
| C5 | Market opportunity | 10% | Workbook (relative rank of commitment value within the country) |

**C1 Materiality**
- 5: The client identifies the topic as material, or it relates to a core business line or large disclosed exposure.
- 4: Clearly linked to a significant business activity or exposure.
- 3: Linked to some activities; moderate relevance.
- 2: Limited or indirect relevance.
- 1: No identifiable relevance in the sources.

**C2 Action gap** (a higher score means more room to develop)
- 5: Material, but no disclosed action.
- 4: General statements or advocacy only.
- 3: Commitments, policies or eligibility criteria, but no quantified nature target.
- 2: Quantified target with limited progress reporting.
- 1: Quantified nature target with reported progress and governance.
- Cap C2 at 2 when C1 is 2 or less.

**C4 Regulatory and market momentum**
- 5: Mandatory requirement in force or confirmed.
- 4: Announced or consulted requirement, or a widely adopted framework.
- 3: Emerging voluntary framework or investor expectation.
- 2: Limited drivers.
- 1: None identified.

**Calculated by the workbook**
- Weighted score to one decimal place.
- Tier: High is 4.0 or above, Medium is 3.0 to 3.9, Low is below 3.0.
- Tie-break: higher C1, then higher C2, then more client evidence.
- Confidence: High needs 3 or more client items from 2 or more sources, with at least one Direct mapping. Medium needs at least 1 client item. Low means no client evidence.

---

## D. Slide specification (EY content slide; max 3 slides plus the template cover)

| Slide | Title (≤75 chars, action title) | Content |
|---|---|---|
| 1 Executive summary | Overall finding | Three stat callouts (largest nature-related exposure figure; number of High sub-themes "x of 15"; one gap figure). Body ≤120 words: Context (1 bullet), Key findings (3 bullets: exposure, action, gap), Top priorities (High sub-themes with scores), Implication (1 bullet). |
| 2 Priorities | Pattern in the ranking | Table: # · Sub-theme · NBSAP value and number · Client exposure · Score · Tier · Key client evidence (≤12 words). All High and Medium rows (max 12). Low sub-themes listed in a panel below with scores. Footnote: weights and data caveats. |
| 3 How EY can assist | What EY could help the client do | Four cards (top four priorities; merge two water sub-themes if both rank). Each card has three parts: What the public evidence shows · How EY could support · Indicative output. Then one "Across all four" line and Proposed next steps (3). Support areas are labelled as "to be aligned with the EY CCaSS service catalogue". |

Every slide has the client line "<Client> | <section>" and speaker notes in this form:
```
[Sources]
<document, pages/tabs>
[/Sources]
```

---

## E. QA checklist (Step 6)

1. All sources are listed. Any source older than 24 months is flagged.
2. Every Evidence_Log row has a source, a page or tab, and an exact quote or figure.
3. Every evidence row appears in NBSAP_Mapping, with valid IDs (or XC).
4. All 15 sub-themes have C1, C2 and C4 values, plus a rationale.
5. The C2 cap is respected.
6. Exposure figures match the source exactly, including currency and date.
7. No external knowledge is used. Interpretation is labelled "EY view".
8. UK English and neutral tone throughout.
