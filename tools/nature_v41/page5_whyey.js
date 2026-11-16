// Skeleton page 5 (v5.0): "Why EY" – headings and [EY team] placeholders only. Written by the EY team, never by the agent.
const pptxgen = require('pptxgenjs');
const C = { FONT: 'EYInterstate Light', FB: 'EYInterstate', Y: 'FFE600', WH: 'F2F2F2', GR: 'BFBFC7', CARD: '2E2E38', LINE: '3A3A46', DARK: '1A1A24' };
const tx = (o) => Object.assign({ isTextBox: true, margin: 0, valign: 'top', fontFace: C.FONT, color: C.WH }, o);
const pres = new pptxgen(); pres.layout = 'LAYOUT_WIDE'; const s = pres.addSlide();
const LX = 0.58, CW = (11.46 - 2 * 0.2) / 3;
for (let n = 1; n <= 3; n++) {
  const x = LX + (n - 1) * (CW + 0.2);
  s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y: 1.3, w: CW, h: 2.9, fill: { color: C.CARD }, line: { color: C.LINE, width: 0.75 }, rectRadius: 0.08, objectName: `card-${n}` });
  s.addShape(pres.shapes.OVAL, { x: x + 0.2, y: 1.5, w: 0.45, h: 0.45, fill: { color: C.Y }, line: { type: 'none' }, objectName: `card-chip-${n}` });
  s.addText(String(n), tx({ x: x + 0.2, y: 1.5, w: 0.45, h: 0.45, fontSize: 14, bold: true, fontFace: C.FB, color: C.DARK, align: 'center', valign: 'middle' }));
  s.addText(`[EY team: heading ${n}]`, tx({ x: x + 0.8, y: 1.5, w: CW - 1.0, h: 0.45, fontSize: 12, bold: true, fontFace: C.FB, color: C.Y, valign: 'middle', objectName: `card-head-${n}` }));
  s.addText(`[EY team: placeholder ${n}]`, tx({ x: x + 0.2, y: 2.15, w: CW - 0.4, h: 1.9, fontSize: 10.5, color: C.GR, objectName: `card-body-${n}` }));
}
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX, y: 4.45, w: 11.46, h: 0.8, fill: { color: C.CARD }, line: { color: C.Y, width: 1 }, rectRadius: 0.06, objectName: 'proposal-panel' });
s.addText('[EY team: proposal / next step]', tx({ x: LX + 0.2, y: 4.5, w: 11.06, h: 0.7, fontSize: 11, color: C.GR, valign: 'middle', objectName: 'proposal' }));
s.addText('[EY team: contacts]', tx({ x: LX, y: 5.45, w: 11.46, h: 0.3, fontSize: 10, color: C.GR, objectName: 'contacts' }));
s.addText('[EY team: sources, if any]', tx({ x: LX, y: 6.58, w: 11.46, h: 0.3, fontSize: 8, italic: true, color: C.GR, objectName: 'footnote' }));
s.addText('[EY team: abbreviations used on this page, if any]', tx({ x: 0.58, y: 6.87, w: 11.46, h: 0.26, fontSize: 6.5, color: C.GR, objectName: 'abbreviation-key' }));
pres.writeFile({ fileName: process.argv[2] }).then(() => console.log('page 5 ok'));
