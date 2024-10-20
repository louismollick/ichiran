import childProcess from "child_process";
import type { IchiranResponse } from "../../types/ichiran";

const command = "ichiran-cli";

// Verify ichiran-cli is alive and working
export const doHealthCheck = () => {
  const result = childProcess.spawnSync(command, ["-h"], {
    encoding: "utf8",
  });

  if (result.error || result.stderr) {
    const message = `ichiran-cli not working as expected".\nError: ${result.error} ${result.stderr}`;
    console.error(message);
    throw new Error(message);
  }
};

export const segmentText = (input: string) => {
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
