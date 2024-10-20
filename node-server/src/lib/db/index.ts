import { sql } from 'drizzle-orm';
import { drizzle } from 'drizzle-orm/node-postgres';

const db = drizzle(process.env.DATABASE_URL!);

export async function getCommonWordsForKanji(kanji: string) {
  const result = await db.execute(sql`
    SELECT * FROM public.gloss g 
    JOIN public.sense s ON g.sense_id=s.id 
    JOIN public.kanji_text k ON s.seq=k.seq
    WHERE k.text LIKE  '%${kanji}%' AND common IS NOT NULL
    ORDER BY k.text, s.ord, g.ord ASC;
    `)

  return JSON.stringify(result)
}
