import dotenv from "dotenv";
import { sql } from "drizzle-orm";
import { drizzle } from "drizzle-orm/node-postgres";

dotenv.config();

const db = drizzle(process.env.DATABASE_URL!);

export async function getCommonWordsForKanji(kanji: string) {
  const result = await db.execute(sql`
    WITH readings AS (
      SELECT k.id, json_agg(jsonb_build_object(
        'kana', r.text,
        'type', r.type,
        'num_words', r.stat_common, 
        'perc', (r.stat_common * 100.0 / k.stat_common)
      ) ORDER BY r.stat_common DESC) as readings
      FROM public.kanji k 
      JOIN public.reading r ON k.id=r.kanji_id
      WHERE k.text IN (${kanji}) AND r.type <> 'ja_na'
      GROUP BY k.id
    ),
    meanings AS (
      SELECT k.id, json_agg(m.text ORDER BY m.id) as meanings
      FROM public.kanji k
      JOIN public.meaning m ON k.id=m.kanji_id
      WHERE k.text IN (${kanji})
      GROUP BY k.id
    )
    SELECT k.id, k.text, k.freq, k.grade, k.strokes, k.stat_common as total_words, r.readings, m.meanings
    FROM public.kanji k
    JOIN meanings m ON k.id=m.id
    JOIN readings r ON k.id=r.id;`);
  return result.rows;
}
