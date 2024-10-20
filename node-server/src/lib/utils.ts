import { promises as fs } from "fs";
import type { MokuroResponse } from "../types/mokuro";

export function parseError(error: unknown) {
  return JSON.stringify(error, Object.getOwnPropertyNames(error));
}

export async function safelyReadFile(filePath: string) {
  try {
    const ocrFile = await fs.readFile(filePath, "utf8");
    return JSON.parse(ocrFile) as MokuroResponse;
  } catch (error) {
    console.log(
      `Error during safelyReadFile for filePath ${filePath}.\nError: ${parseError(error)}`,
    );
    return null;
  }
}

export async function safelyWriteFile(filePath: string, content: string) {
  try {
    await fs.writeFile(filePath, content);
    return true;
  } catch (error) {
    console.log(
      `Error during safelyWriteFile for filePath ${filePath}.\nError: ${parseError(error)}`,
    );
    return false;
  }
}
