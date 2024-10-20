import type { IchiranResponse } from "./ichiran";

export type MokuroResponse = {
  version: string;
  img_width: number;
  img_height: number;
  blocks: Block[];
};

export type Block = {
  box: [number, number, number, number];
  vertical: boolean;
  font_size: number;
  lines_coords: [number, number][][];
  lines: string[];
  segmentation: IchiranResponse;
};
