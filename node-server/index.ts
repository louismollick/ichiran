import Fastify, { RequestGenericInterface } from 'fastify'
import helmet from '@fastify/helmet'
import cors from '@fastify/cors'

import childProcess from "child_process";
import dotenv from "dotenv";
import { promises as fs } from "fs";

import type { MokuroResponse } from "./types/mokuro";
import type { IchiranResponse } from "./types/ichiran";

dotenv.config();
const PORT = parseInt(process.env.PORT!) || 3000;
const fastify = Fastify({
  logger: true,
});
await fastify.register(helmet);
await fastify.register(cors);

function parseError(error: unknown) {
  return JSON.stringify(error, Object.getOwnPropertyNames(error))
}

async function safelyReadOcrFile(filePath: string) {
  try {
    const ocrFile = await fs.readFile(filePath, "utf8");
    return JSON.parse(ocrFile) as MokuroResponse;
  } catch (error) {
    console.log(
      `Error during safelyReadOcrFile for filePath ${filePath}.\nError: ${parseError(error)}`,
    );
    return null;
  }
}

async function safelyWriteOcrFile(filePath: string, content: string) {
  try {
    await fs.writeFile(filePath, content);
    return true;
  } catch (error) {
    console.log(
      `Error during safelyWriteOcrFile for filePath ${filePath}.\nError: ${parseError(error)}`,
    );
    return false;
  }
}

const command = "ichiran-cli";

// Verify ichiran-cli is alive and working
const doHealthCheck = () => {
  const result = childProcess.spawnSync(command, ["-h"], {
    encoding: "utf8",
  });

  if (result.error || result.stderr) {
    const message = `ichiran-cli not working as expected".\nError: ${result.error} ${result.stderr}`;
    console.error(message);
    throw new Error(message);
  }
};

const segmentText = (input: string) => {
  const result = childProcess.spawnSync(command, ["-f", input], {
    encoding: "utf8",
  });

  if (result.error || result.stderr) {
    const message = `Error during segmentation of input: "${input}".\nError: ${result.error} ${result.stderr}`;
    console.error(message);
    throw new Error(message);
  }

  return JSON.parse(result.stdout) as IchiranResponse;
};

fastify.get("/", (_, reply) => {
  return reply.status(200).send("Hi!");
});

fastify.get("/health", (_, reply) => {
  console.log(`Beginning ichiran health test...`);

  try {
    doHealthCheck();
    return reply.status(200).send("OK");
  } catch (error) {
    return reply.status(500).send(parseError(error));
  }
});

type SegmentationRequest = RequestGenericInterface & {
  Params: { input: string };
}

fastify.get<SegmentationRequest>("/segment/:input", (request, reply) => {
  const { input } = request.params;
  console.log(`Beginning segmentation of input: ${input}`);
  if (!input) {
    const errorMessage = "Input is empty, sending failure.";
    console.error(errorMessage);
    return reply.status(400).send(`Error: ${errorMessage}`);
  }

  reply.status(200).send(segmentText(input));
});

type VolumeSegmentationRequest = RequestGenericInterface & {
  Params: { mangaSlug: string, volumeNumber: string };
  Querystring: { force?: boolean };
}

fastify.get<VolumeSegmentationRequest>(
  "/segment/:mangaSlug/:volumeNumber",
  async (request, reply) => {
    const { mangaSlug, volumeNumber } = request.params;
    const dirPath = `${process.cwd()}/shared/images/${mangaSlug}/jp-JP/_ocr/volume-${volumeNumber}`;

    console.log(`Beginning segmentation of directory path: ${dirPath}`);

    try {
      doHealthCheck();
    } catch (error) {
      return reply.status(500).send(parseError(error));
    }

    const t0 = performance.now();

    // Read each Mokuro OCR .json file in the directory, and for each speech bubble text call ichiran-cli, then save back to .json file
    const fileNames = await fs.readdir(dirPath);
    const forceResegmentation = request.query.force;
    Promise.all(
      fileNames.map(async (fileName) => {
        const filePath = `${dirPath}/${fileName}`;
        console.info(`Reading ${filePath}...`);
        const ocr = await safelyReadOcrFile(filePath);

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

        const writeSuccess = await safelyWriteOcrFile(
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
    ).then(() => {
      const time = performance.now() - t0;
      console.log(`Segmentation of directory took ${time} milliseconds.`);
    });

    return reply.status(200).send(`Started segmentation of directory path: ${dirPath}`);
  },
);

await fastify.listen({ port: PORT }, () => console.log("Server running at PORT: ", PORT));