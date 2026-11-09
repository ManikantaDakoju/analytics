// Skeleton page 2 (v4.1): context band + where the money goes (Chart A) + where the country is heading. Placeholders only.
const pptxgen = require('pptxgenjs');
const D = JSON.parse(require('fs').readFileSync(process.argv[2], 'utf8')); // sample chart data (LBG example)
const C = { FONT: 'EYInterstate Light', FB: 'EYInterstate', Y: 'FFE600', WH: 'F2F2F2', GR: 'BFBFC7', CARD: '2E2E38', LINE: '3A3A46', MED: 'A5A5B0', DARK: '1A1A24' };
const tx = (o) => Object.assign({ isTextBox: true, margin: 0, valign: 'top', fontFace: C.FONT, color: C.WH }, o);
const w = (t, o = {}) => ({ text: t, options: Object.assign({ color: C.WH, fontFace: C.FONT }, o) });
const pres = new pptxgen(); pres.layout = 'LAYOUT_WIDE'; const s = pres.addSlide();
const LX = 0.58, LW = 5.55, RX = 6.45, RW = 5.59;
// context band
const BY = 1.27, BH = 0.72, BW = (11.46 - 2 * 0.14) / 3;
[['What it is', '{{s1_ctx_what}}'], ['What we measured', '{{s1_ctx_measured}}'], ['Why it matters', '{{s1_ctx_why}}']].forEach((b, i) => {
  const x = LX + i * (BW + 0.14);
  s.addShape(pres.shapes.RECTANGLE, { x, y: BY, w: 0.05, h: BH, fill: { color: C.Y }, line: { type: 'none' }, objectName: `ctx-bar-${i + 1}` });
  s.addText([w(b[0], { bold: true, fontFace: C.FB, color: C.Y, fontSize: 10, breakLine: true }), w(b[1], { fontSize: 9.5 })], tx({ x: x + 0.14, y: BY, w: BW - 0.18, h: BH, objectName: `ctx-${i + 1}` }));
});
const TOP = 2.12;
s.addText([w('{{s1_left_head}}', { bold: true, fontFace: C.FB, color: C.Y, fontSize: 13 })], tx({ x: LX, y: TOP, w: LW, h: 0.28, objectName: 'left-heading' }));
s.addText('{{s1_left_sub}}', tx({ x: LX, y: TOP + 0.3, w: LW, h: 0.22, fontSize: 9.5, color: C.GR, objectName: 'left-sub' }));
const labels = D.A.map((r) => r[0]);
s.addChart(pres.charts.BAR, [{ name: 'Top 2', labels, values: D.A.map((r) => r[1]) }, { name: 'Other', labels, values: D.A.map((r) => r[2]) }], {
  x: LX - 0.05, y: TOP + 0.56, w: LW + 0.1, h: 2.08, barDir: 'bar', barGrouping: 'clustered', barOverlapPct: 100, barGapWidthPct: 55,
  chartColors: [C.Y, C.MED], catAxisOrientation: 'maxMin', valAxisHidden: true, valAxisMinVal: 0, valGridLine: { style: 'none' }, catGridLine: { style: 'none' }, catAxisLineShow: false,
  catAxisLabelColor: C.WH, catAxisLabelFontFace: C.FONT, catAxisLabelFontSize: 10, showValue: true, dataLabelPosition: 'outEnd', dataLabelColor: C.WH, dataLabelFontFace: C.FB, dataLabelFontSize: 10, dataLabelFontBold: true,
  dataLabelFormatCode: '0.0"bn";-0.0;;', showLegend: false, objectName: 'chart-A-money-by-theme' });
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX, y: TOP + 2.74, w: LW, h: 0.56, fill: { color: C.CARD }, line: { color: C.Y, width: 1 }, rectRadius: 0.06, objectName: 'callout' });
s.addText([w('{{s1_callout_stat}}  ', { bold: true, fontFace: C.FB, color: C.Y, fontSize: 19 }), w('{{s1_callout_text}}', { fontSize: 11 })], tx({ x: LX + 0.16, y: TOP + 2.75, w: LW - 0.3, h: 0.54, valign: 'middle', objectName: 'callout-text' }));
s.addShape(pres.shapes.LINE, { x: 6.27, y: TOP + 0.05, w: 0, h: 3.7, line: { color: C.LINE, width: 1 } });
s.addText([w('{{s1_heading_r}}', { bold: true, fontFace: C.FB, color: C.Y, fontSize: 13 })], tx({ x: RX, y: TOP, w: RW, h: 0.28, objectName: 'right-heading' }));
s.addText('{{s1_sub_r}}', tx({ x: RX, y: TOP + 0.3, w: RW, h: 0.22, fontSize: 9.5, color: C.GR, objectName: 'right-sub' }));
const TW = (RW - 0.14) / 2, TH = 1.08;
[1, 2, 3, 4].forEach((n, i) => {
  const x = RX + (i % 2) * (TW + 0.14), y = TOP + 0.6 + Math.floor(i / 2) * (TH + 0.12);
  s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y, w: TW, h: TH, fill: { color: C.CARD }, line: { color: C.LINE, width: 0.75 }, rectRadius: 0.08, objectName: `tile-${n}` });
  s.addText(`{{s1_tile${n}_stat}}`, tx({ x: x + 0.14, y: y + 0.07, w: TW - 0.28, h: 0.4, fontSize: 20, bold: true, fontFace: C.FB, color: C.Y, objectName: `tile-${n}-stat` }));
  s.addText(`{{s1_tile${n}_text}}`, tx({ x: x + 0.14, y: y + 0.5, w: TW - 0.28, h: 0.55, fontSize: 9.5, objectName: `tile-${n}-text` }));
});
s.addText([w('{{s1_rules_lead}} ', { bold: true, fontFace: C.FB }), w('{{s1_rules}}', { color: C.GR })], tx({ x: RX, y: TOP + 3.06, w: RW, h: 0.42, fontSize: 9.5, objectName: 'rules' }));
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX, y: 6.0, w: 11.46, h: 0.52, fill: { color: C.Y }, line: { type: 'none' }, rectRadius: 0.06, objectName: 'so-what-panel' });
s.addText([w('So what  ', { bold: true, fontFace: C.FB, color: C.DARK }), w('{{s1_sowhat}}', { color: C.DARK })], tx({ x: LX + 0.2, y: 6.02, w: 11.06, h: 0.48, fontSize: 11.5, valign: 'middle', objectName: 'so-what' }));
s.addText('{{s1_footnote}}', tx({ x: LX, y: 6.58, w: 11.46, h: 0.36, fontSize: 8.5, italic: true, color: C.GR, objectName: 'footnote' }));
pres.writeFile({ fileName: process.argv[3] }).then(() => console.log('page 2 ok'));
