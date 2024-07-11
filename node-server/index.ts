import express, { type Request, type Response } from "express";
import childProcess from "child_process";
import dotenv from "dotenv";
import helmet from "helmet";
import cors from 'cors';

dotenv.config();
const app = express();
const PORT = process.env.PORT || 3000;

app.use(helmet());
app.use(cors());

app.get("/:input", (request: Request, response: Response) => {
  const input = request.params.input.trim();
  console.log(`Received input: ${input}`)
  if (!input) {
    const errorMessage = "Input is empty, sending failure.";
    console.error(errorMessage);
    return response.status(400).send(`Error: ${errorMessage}`);
  }

  const result = childProcess.spawnSync("ichiran-cli", ["-f", input], {
    encoding: "utf8",
  });

  if (result.error) {
    console.error(result.error);
    return response.status(500).send(JSON.stringify(result.error));
  }

  response.setHeader('Content-Type', 'application/json');
  return response.status(200).send(result.stdout);
});

app.listen(PORT, () => console.log("Server running at PORT: ", PORT));
