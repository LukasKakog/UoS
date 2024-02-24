using UnityEngine;
using UnityEngine.AI;

public class LionController : MonoBehaviour {
    private NavMeshAgent navMeshAgent;
    private Animator animator;
    public PlayerController player;

    public MonoBehaviour levelController;
    public LevelController levelController2;
    public LayerMask playerLayer;

    public bool inCutScene = false;

    void Start() {
        navMeshAgent = GetComponent<NavMeshAgent>();
        animator = GetComponent<Animator>();

        levelController2 = levelController as LevelController;
        if (levelController2 == null) {
            Debug.LogError("Assigned controller does not implement LevelController.");
        }
    }

    public void OnCutSceneStart() {
        animator.enabled = false;
        navMeshAgent.isStopped = true;
        inCutScene = true;
    }

    public void OnCutSceneEnd() {
        animator.enabled = true;
        navMeshAgent.isStopped = false;
        inCutScene = false;
    }

    void Update() {
        if (!inCutScene) {
            navMeshAgent.SetDestination(player.transform.position);
            navMeshAgent.speed = 4;
            animator.SetBool("isWalking", true);
        }
    }

    void OnTriggerEnter(Collider collider) {
        if (((1 << collider.gameObject.layer) & playerLayer) != 0) {
            levelController2.StartSecondCutScene();
        }
    }
}
