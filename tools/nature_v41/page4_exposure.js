// Skeleton page 4 (v5.0): client portfolio exposure panel (chart D) + condensed today / not yet visible / opportunity grid. Placeholders only.
const pptxgen = require('pptxgenjs');
const D = JSON.parse(require('fs').readFileSync(process.argv[2], 'utf8'));
const C = { FONT: 'EYInterstate Light', FB: 'EYInterstate', Y: 'FFE600', WH: 'F2F2F2', GR: 'BFBFC7', CARD: '2E2E38', LINE: '3A3A46', MED: '747480', DARK: '1A1A24' };
const tx = (o) => Object.assign({ isTextBox: true, margin: 0, valign: 'top', fontFace: C.FONT, color: C.WH }, o);
const w = (t, o = {}) => ({ text: t, options: Object.assign({ color: C.WH, fontFace: C.FONT }, o) });
const pres = new pptxgen(); pres.layout = 'LAYOUT_WIDE'; const s = pres.addSlide();
const LX = 0.58, TOP = 1.25, PW = 4.05;
// ---- left: exposure panel
s.addText('{{client_short}}’s book by sector', tx({ x: LX, y: TOP, w: PW, h: 0.26, fontSize: 11, bold: true, fontFace: C.FB, color: C.Y, objectName: 'exp-heading' }));
s.addText('{{s3_exp_sub}}', tx({ x: LX, y: TOP + 0.28, w: PW, h: 0.3, fontSize: 8, color: C.GR, objectName: 'exp-sub' }));
const lab = D.D.map((r) => r[0]);
s.addChart(pres.charts.BAR, [{ name: 'Nature-priority part', labels: lab, values: D.D.map((r) => r[1]) }, { name: 'Rest of sector', labels: lab, values: D.D.map((r) => r[2]) }], {
  x: LX - 0.06, y: TOP + 0.6, w: PW + 0.1, h: 3.05, barDir: 'bar', barGrouping: 'stacked', barGapWidthPct: 45, catAxisOrientation: 'maxMin',
  catAxisLabelFontFace: C.FONT, catAxisLabelFontSize: 7.5, catAxisLabelColor: C.WH, catAxisLineShow: false, valAxisHidden: true, valAxisMinVal: 0,
  valGridLine: { style: 'none' }, catGridLine: { style: 'none' }, showLegend: false, chartColors: [C.Y, C.MED], objectName: 'chart-D-book' });
s.addShape(pres.shapes.RECTANGLE, { x: LX, y: TOP + 3.72, w: 0.12, h: 0.12, fill: { color: C.Y }, line: { type: 'none' }, objectName: 'exp-legend-chip' });
s.addText('Bars: share of total exposure; yellow = nature-priority part, as disclosed', tx({ x: LX + 0.18, y: TOP + 3.69, w: PW - 0.18, h: 0.2, fontSize: 7.5, color: C.GR, objectName: 'exp-legend' }));
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX, y: TOP + 3.97, w: PW, h: 0.66, fill: { color: C.CARD }, line: { color: C.Y, width: 1 }, rectRadius: 0.06, objectName: 'exp-callout' });
s.addText([w('{{s3_exp_stat}}', { bold: true, fontFace: C.FB, color: C.Y, fontSize: 17 })], tx({ x: LX + 0.12, y: TOP + 4.0, w: 0.82, h: 0.6, valign: 'middle', objectName: 'exp-stat' }));
s.addText('{{s3_exp_text}}', tx({ x: LX + 0.98, y: TOP + 4.0, w: PW - 1.08, h: 0.6, fontSize: 8.5, valign: 'middle', objectName: 'exp-text' }));
s.addShape(pres.shapes.LINE, { x: LX + PW + 0.17, y: TOP, w: 0, h: 4.63, line: { color: C.LINE, width: 1 }, objectName: 'divider' });
// ---- right: grid
const GX = LX + PW + 0.34, LW = 0.98, GAP = 0.1, CX = GX + LW + 0.06, CW = (12.04 - CX - 2 * GAP) / 3;
const ROWS = [
  { key: 'today', y: TOP + 0.6, h: 1.42, label: ['{{client_short}} today', 'What public disclosures show'], opp: false },
  { key: 'gap', y: TOP + 2.1, h: 1.2, label: ['Not yet visible', 'In the documents reviewed'], opp: false },
  { key: 'opp', y: TOP + 3.38, h: 1.25, label: ['Opportunity', 'EY view'], opp: true }];
for (let c = 1; c <= 3; c++) {
  const x = CX + (c - 1) * (CW + GAP);
  s.addShape(pres.shapes.RECTANGLE, { x, y: TOP, w: CW, h: 0.5, fill: { color: C.Y }, line: { type: 'none' }, objectName: `col-head-${c}` });
  s.addText(`{{h${c}}}`, tx({ x: x + 0.1, y: TOP, w: CW - 0.2, h: 0.5, fontSize: 10, bold: true, fontFace: C.FB, color: C.DARK, valign: 'middle', objectName: `col-head-text-${c}` }));
}
ROWS.forEach((r, i) => {
  s.addText([w(r.label[0], { bold: true, fontFace: C.FB, fontSize: 9.5, color: r.opp ? C.Y : C.WH, breakLine: true }), w(r.label[1], { fontSize: 7.5, color: C.GR })],
    tx({ x: GX, y: r.y + 0.06, w: LW, h: r.h - 0.1, objectName: `row-label-${i + 1}` }));
  for (let c = 1; c <= 3; c++) {
    const x = CX + (c - 1) * (CW + GAP);
    s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y: r.y, w: CW, h: r.h, fill: { color: r.opp ? C.DARK : C.CARD }, line: r.opp ? { color: C.Y, width: 1 } : { type: 'none' }, rectRadius: 0.05, objectName: `cell-${i + 1}-${c}` });
    s.addText(`{{s3_col${c}_${r.key}}}`, tx({ x: x + 0.1, y: r.y + 0.07, w: CW - 0.2, h: r.h - 0.12, fontSize: 9, objectName: `cell-text-${i + 1}-${c}` }));
  }
});
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX, y: 6.0, w: 11.46, h: 0.52, fill: { color: C.Y }, line: { type: 'none' }, rectRadius: 0.06, objectName: 'so-what-panel' });
s.addText([w('So what  ', { bold: true, fontFace: C.FB, color: C.DARK }), w('{{s3_sowhat}}', { color: C.DARK })], tx({ x: LX + 0.2, y: 6.02, w: 11.06, h: 0.48, fontSize: 11, valign: 'middle', objectName: 'so-what' }));
s.addText('{{s3_footnote}}', tx({ x: LX, y: 6.58, w: 11.46, h: 0.3, fontSize: 8, italic: true, color: C.GR, objectName: 'footnote' }));
s.addText('{{s3_key}}', tx({ x: 0.58, y: 6.87, w: 11.46, h: 0.26, fontSize: 6.5, color: C.GR, objectName: 'abbreviation-key' }));
pres.writeFile({ fileName: process.argv[3] }).then(() => console.log('page 4 ok'));
