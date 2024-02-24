using System;
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;

public class TerrainGen : MonoBehaviour {
    [Header("Terrain Settings")]
    public float perlinScale = 10f;
    public float pointDensity = 1;
    public int outerPoints;

    public int terrainRadius = 200;
    public int TerrainHeight = 1;

    public AnimationCurve terrainBumpiness;

    [Header("Cliff Settings")]
    public bool spawnCliffs = true;
    public GameObject cliffParent;
    public GameObject cliffSide;
    public int cliffCount;

    [Header("Vegetation Settings")]
    public bool spawnSpawnables = true;
    public SpawnableRecord[] spawnables;
    private MeshGenerator meshGenerator;

    void Start() {
        meshGenerator = new MeshGenerator();
        meshGenerator.Initialise(
            perlinScale,
            terrainBumpiness,
            terrainRadius,
            TerrainHeight,
            pointDensity,
            outerPoints,
            gameObject.GetComponent<MeshFilter>(),
            gameObject.GetComponent<MeshCollider>());

        if (spawnCliffs) {
            GenerateCliffs();
        }

        if (spawnSpawnables) {
            SpawnSpawnables();
        }
    }

    void GenerateCliffs() {
        for (int i = 0; i < cliffCount; i++) {
            float angle = i * (360f / cliffCount); // Calculate angle for each object
            float x = Mathf.Cos(Mathf.Deg2Rad * angle) * terrainRadius;
            float z = Mathf.Sin(Mathf.Deg2Rad * angle) * terrainRadius;

            Vector3 position = new Vector3(x, 0f, z) + cliffParent.transform.position;
            GameObject newCliff = Instantiate(cliffSide, position, Quaternion.identity);
            newCliff.transform.LookAt(cliffParent.transform.position);
        }
    }

    void SpawnSpawnables() {
        foreach (SpawnableRecord spawnableRecord in spawnables) {
            int spawnCount = (int) math.ceil(0.5 * math.PI * terrainRadius * terrainRadius * spawnableRecord.density);
            List<Vector3> spawnedCoords = new List<Vector3>();
            int attempts = 0;

            while (spawnedCoords.Count < spawnCount) {
                float angle = UnityEngine.Random.Range(0f, 2 * math.PI);
                float distance = (float) Math.Sqrt(UnityEngine.Random.Range(0f, (float) (terrainRadius * terrainRadius)));

                float x = math.cos(angle) * distance;
                float y = 20;
                float z = math.sin(angle) * distance;
                Vector3 position = new Vector3(x, y, z);

                float bumpiness = terrainBumpiness.Evaluate(math.clamp((x + terrainRadius) / (2 * terrainRadius), 0, 1));

                // Too close to farm
                if (bumpiness < 0.1) {
                    continue;
                }

                if (attempts > 1000) {
                    Debug.Log("TOO MANY SPAWN ATTEMPTS");
                    spawnCount = 0;
                    continue;
                }

                RaycastHit rayHitInfo;
                if (Physics.SphereCast(position, math.pow(math.pow(bumpiness, 1.8f) * spawnableRecord.density * 4, -0.5f), Vector3.down, out rayHitInfo, 25)) {
                    if (rayHitInfo.collider.gameObject != gameObject) {
                        attempts += 1;
                        continue;
                    }

                    if (Physics.Raycast(position, Vector3.down, out rayHitInfo, 25)) {
                        position = rayHitInfo.point;
                        if (rayHitInfo.point.y > TerrainHeight) {
                            attempts += 1;
                            continue;
                        }
                    } else {
                        position.y -= rayHitInfo.distance + math.pow(math.pow(bumpiness, 1.8f) * spawnableRecord.density * 4, -0.5f);
                    }
                } else {
                    attempts += 1;
                    continue;
                }

                attempts = 0;
                spawnedCoords.Add(position);
                Instantiate(spawnableRecord.GetRandomObject(), position, Quaternion.Euler(0, UnityEngine.Random.Range(0, 360), 0));
            }
        }
    }
}

[System.Serializable]
public class SpawnableRecord {
    public GameObject[] objects;
    public float density;

    public GameObject GetRandomObject() {
        int randomIndex = UnityEngine.Random.Range(0, objects.Length);
        return objects[randomIndex];
    }
}