using UnityEngine;

public class StartSecondCutScene : MonoBehaviour {
    public MonoBehaviour levelController;
    public LevelController levelController2;
    public LayerMask playerLayer;

    void Start() {
        levelController2 = levelController as LevelController;

        if (levelController2 == null) {
            Debug.LogError("Assigned controller does not implement LevelController.");
        }
    }

    void OnTriggerEnter(Collider collider) {
        if (((1 << collider.gameObject.layer) & playerLayer) != 0) {
            levelController2.StartSecondCutScene();
        }
    }
}
