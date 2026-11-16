# Prompt 6 – Glossary rows (paste from row 60, columns A:G; G = Key text) and Calculations (paste from row 30)

| ID | Term | Type | Definition / expansion (as used on slides) | Source_ID | Reference (page/tab) | Key text (shown in the abbreviation key at the foot of the page, e.g. "UK = United Kingdom") |
|---|---|---|---|---|---|---|
| ABB-LBG | LBG | Abbreviation | Lloyds Banking Group | SRC-02 | PDF p.2 (title page and throughout) | LBG = Lloyds Banking Group |
| ABB-UK | UK | Abbreviation | United Kingdom | SRC-EY | Countries tab (GBR) | UK = United Kingdom |
| ABB-BNG | BNG | Abbreviation | Biodiversity Net Gain: 10% gain required for new developments in England since February 2024 | SRC-01 | p.7 | BNG = biodiversity net gain |
| ABB-PRA | PRA | Abbreviation | Prudential Regulation Authority, Bank of England (written "Prudential Regulatory Authority" in the source) | SRC-02 | PDF p.11 | PRA = Prudential Regulation Authority |
| ABB-SS525 | SS5/25 | Abbreviation | PRA Supervisory Statement 5/25 (December 2025) on physical climate and transition risk | SRC-02 | PDF p.11 | SS5/25 = Supervisory Statement 5/25 |
| ABB-HARP | HARP | Abbreviation | Haweswater Aqueduct Resilience Programme | SRC-02 | PDF p.14 | HARP = Haweswater Aqueduct Resilience Programme |
| ABB-KPI | KPI | Abbreviation | Key performance indicator | SRC-METH | Section 9 (standard terms) | KPI = key performance indicator |
| DEF-NOTATION | £bn, £m | Definition | £bn = billion pounds sterling; £m = million pounds sterling (the abbreviations "c.", "e.g." and "ha" are no longer used on the slides) | SRC-METH | Section 9 (style rules) |  |
| DEF-EXPOSURE | Exposure / LBG’s book | Definition | Total exposure = lending (loans and advances before impairment) + off-balance sheet (commitments, guarantees, contingent liabilities). "LBG’s book" = Group total exposure, £645,058m at 31 Dec 2025 | SRC-04 | Tab 18, footnotes 3 and 4 |  |
| DEF-NATPRIOR | Nature priority sector | Definition | Sector LBG identifies as a priority for nature, separately from increased climate risk | SRC-04 | Tab 18, footnote 12 |  |
| DEF-NOTVISIBLE | Questions to explore (not yet visible) | Definition | Not found in the four LBG documents reviewed (SRC-01, 02, 04, 05); may be covered in reports not reviewed | SRC-METH | Section 2 (absence checks) |  |
| DEF-OUTPUT | Economic output highly dependent on nature | Definition | Share of UK gross value added (GVA) from domestic production in activities with a high nature-dependency rating, as modelled by ENCORE Insights (Global Canopy) | SRC-10 | country_headline_figures.csv; ReadMe.txt |  |
| ABB-ENC2 | ENCORE Insights | Definition | ENCORE Insights: Country Dashboard by Global Canopy (ENCORE = Exploring Natural Capital Opportunities, Risks and Exposure) | SRC-10 | ReadMe.txt |  |
| ABB-AI | AI | Abbreviation | Artificial Intelligence | SRC-08 | p.6 ("data centres and Artificial Intelligence") | AI = artificial intelligence |

| CALC_ID | Description | Value | Unit | How it is calculated | Input trace IDs | Source_ID |
|---|---|---|---|---|---|---|
| CALC-51 | LBG Group total exposure (£bn) | =VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE())/1000 | bn | Group total £m ÷ 1,000 | EV-067 | SRC-04 |
| CALC-52 | Mortgages as % of Group exposure | =VLOOKUP("EV-040",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 365,599 ÷ 645,058 | EV-040, EV-067 | SRC-04 |
| CALC-53 | Property companies as % of Group exposure | =VLOOKUP("EV-068",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 26,270 ÷ 645,058 | EV-068, EV-067 | SRC-04 |
| CALC-54 | Energy and water supply as % of Group exposure | =VLOOKUP("EV-038",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 12,084 ÷ 645,058 | EV-038, EV-067 | SRC-04 |
| CALC-55 | Agriculture, forestry and fishing as % of Group exposure | =VLOOKUP("EV-075",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 6,776 ÷ 645,058 | EV-075, EV-067 | SRC-04 |
| CALC-56 | Manufacturing as % of Group exposure | =VLOOKUP("EV-045",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 13,095 ÷ 645,058 | EV-045, EV-067 | SRC-04 |
| CALC-57 | Construction as % of Group exposure | =VLOOKUP("EV-039",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 5,850 ÷ 645,058 | EV-039, EV-067 | SRC-04 |
| CALC-58 | Financial, business and other services as % of Group exposure | =VLOOKUP("EV-099",Client_Exposure!$A:$E,5,FALSE())/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE()) | % | 75,615 ÷ 645,058 | EV-099, EV-067 | SRC-04 |
| CALC-59 | Nature priority sectors as % of Group exposure | =VLOOKUP("EV-034",Client_Exposure!$A:$E,5,FALSE)/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE) | % | 59,991 ÷ 645,058 | EV-034, EV-067 | SRC-04 |
