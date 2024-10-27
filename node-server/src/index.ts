import express, { type Request, type Response } from "express";
import dotenv from "dotenv";
import helmet from "helmet";
import cors from "cors";
import { promises as fs } from "fs";

import { parseError, safelyReadFile, safelyWriteFile } from "./lib/utils";
import { doHealthCheck, segmentText } from "./lib/segmentation";
import { getCommonWordsForKanji } from "./lib/db";

dotenv.config();
const app = express();
const PORT = process.env.PORT || 3000;

app.use(helmet());
app.use(cors());

app.get("/", (_: Request, response: Response) => {
  return response.status(200).send("Hi!");
});

app.get("/health", (_: Request, response: Response) => {
  console.log(`Beginning ichiran health test...`);

  try {
    doHealthCheck();
    return response.status(200).send("OK");
  } catch (error) {
    return response.status(500).send(parseError(error));
  }
});

app.get("/segment/:input", (request: Request, response: Response) => {
  const input = request.params.input.trim();
  console.log(`Beginning segmentation of input: ${input}`);
  if (!input) {
    const errorMessage = "Input is empty, sending failure.";
    console.error(errorMessage);
    return response.status(400).send(`Error: ${errorMessage}`);
  }

  response.status(200).json(segmentText(input));
});

app.get(
  "/kanji/common-words/:input",
  async (request: Request, response: Response) => {
    const input = request.params.input.trim();
    console.log(`Getting common-words for Kanji: ${input}`);
    if (!input) {
      const errorMessage = "Input is empty, sending failure.";
      console.error(errorMessage);
      return response.status(400).send(`Error: ${errorMessage}`);
    }

    try {
      const result = await getCommonWordsForKanji(input);
      response.status(200).json(result);
    } catch (error) {
      response.status(500).json(error);
    }
  },
);

app.get(
  "/segment/:mangaSlug/:volumeNumber",
  async (request: Request, response: Response) => {
    const { mangaSlug, volumeNumber } = request.params;
    const dirPath = `/app/images/${mangaSlug}/jp-JP/_ocr/volume-${volumeNumber}`;

    console.log(`Beginning segmentation of directory path: ${dirPath}`);

    try {
      doHealthCheck();
    } catch (error) {
      return response.status(500).send(parseError(error));
    }

    const t0 = performance.now();

    // Read each Mokuro OCR .json file in the directory, and for each speech bubble text call ichiran-cli, then save back to .json file
    const fileNames = await fs.readdir(dirPath);
    const forceResegmentation = request.query.force;
    await Promise.all(
      fileNames.map(async (fileName) => {
        const filePath = `${dirPath}/${fileName}`;
        console.info(`Reading ${filePath}...`);
        const ocr = await safelyReadFile(filePath);

        if (!ocr) {
          console.error(
            `No OCR found, skipping segmentation for file path: ${filePath}`,
          );
          return;
        }

        console.info(`Starting segmentation for ${filePath}...`);
        ocr.blocks = await Promise.all(
          ocr?.blocks.map((block, blockIdx) => {
            const input = block.lines.join("\n");

            if (!forceResegmentation && block.segmentation) {
              console.log(
                `Skipping segmentation because it was already done, for block ${blockIdx} in file ${filePath}.`,
              );
              return block;
            }

            try {
              block.segmentation = segmentText(input);
            } catch (error) {
              console.error(
                `Error during segmentation. Skipping block ${blockIdx} in file ${filePath}.`,
              );
            }

            return block;
          }),
        );

        const writeSuccess = await safelyWriteFile(
          filePath,
          JSON.stringify(ocr),
        );
        if (!writeSuccess) {
          console.error(
            `Error while writing to OCR file ${filePath}.\nOutput was: ${JSON.stringify(ocr)}`,
          );
          return;
        }

        console.log(`Done segmenting file ${filePath}!`);
      }),
    );

    const time = performance.now() - t0;
    console.log(`Segmentation of directory took ${time} milliseconds.`);

    return response.status(200).send("Done");
  },
);

app.listen(PORT, () => console.log("Server running at PORT: ", PORT));
