# === api.py ===
from flask import Flask, request, jsonify
from sentence_transformers import SentenceTransformer, util
import pandas as pd

app = Flask(__name__)

# Загрузка модели и данных
model = SentenceTransformer('all-MiniLM-L6-v2')
df = pd.read_csv("combined_df_clean.csv", sep=";")
df = df.dropna(subset=["title"])
embeddings = model.encode(df["title"].tolist(), convert_to_tensor=True)

@app.route("/recommend", methods=["POST"])
def recommend():
    data = request.get_json()
    query = data.get("query", "").strip()
    k = int(data.get("k", 5))

    if not query:
        return jsonify([])

    query_embedding = model.encode(query, convert_to_tensor=True)
    cos_scores = util.cos_sim(query_embedding, embeddings)[0]
    top_results = cos_scores.topk(k=k)

    results = []
    for score, idx in zip(top_results[0], top_results[1]):
        item = df.iloc[int(idx)]
        results.append({
            "title": item["title"],
            "author": item.get("author", ""),
            "year": item.get("year", ""),
            "type": item.get("type", ""),
            "similarity": round(float(score), 3)
        })
    return jsonify(results)

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
