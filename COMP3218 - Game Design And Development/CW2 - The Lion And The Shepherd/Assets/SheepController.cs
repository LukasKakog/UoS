using UnityEngine;
using UnityEngine.AI;

public class SheepController : MonoBehaviour {
    private NavMeshAgent navMeshAgent;
    private Animator animator;

    public PlayerController player;

    public Transform movePositionTransform;

    // The slowest and quickest the sheep should move
    public float walkSpeed = 1;
    public float runSpeed = 1;
    public float currentSpeed;

    // How far the sheep can "see". Anything in this area alerts the sheep
    public float viewFOV = 80f; // The sheep's view FOV
    public float viewDistance = 20f; // How far the sheep can see in front of it
    public float detectRadius = 8f; // How far the sheep can detect you from all around it

    private float alertTimer = 0;

    public bool inCutScene = false;

    void Start() {
        navMeshAgent = GetComponent<NavMeshAgent>();
        animator = GetComponent<Animator>();
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
            flee(movePositionTransform);
            animator.Play("walk_forward");
        }
    }

    void flee(Transform fleeTo) {
        float speed;
        if (isInDanger()) {
            speed = runSpeed;
            animator.speed = 3;
            alertTimer = 3;
        } else if (alertTimer > 0) {
            speed = runSpeed;
            animator.speed = 3;
        } else {
            speed = walkSpeed;
            animator.speed = 1;
        }

        alertTimer -= Time.deltaTime;

        navMeshAgent.speed = speed;
        navMeshAgent.SetDestination(fleeTo.position);
    }

    bool isInDanger() {
        return canHearDanger() || canSeeDanger();
    }
    bool canHearDanger() {
        return Vector3.Magnitude(player.transform.position - navMeshAgent.transform.position) < detectRadius;
    }

    bool canSeeDanger() {
        if (Vector3.Magnitude(player.transform.position - navMeshAgent.transform.position) < viewDistance) {
            Vector3 playerDirection = player.transform.position - navMeshAgent.transform.position;
            float playerAngle = Vector3.Angle(navMeshAgent.transform.forward, playerDirection);

            return playerAngle < viewFOV * 0.5f;
        } else {
            return false;
        }
    }
}
