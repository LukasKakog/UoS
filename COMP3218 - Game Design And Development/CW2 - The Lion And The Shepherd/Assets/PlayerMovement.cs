using UnityEngine;

public class PlayerMovement {
    private float moveSpeed;
    private float jumpSpeed;
    private float rotationSpeed;

    private bool forceRotation;

    private Animator animator;
    private CharacterController characterController;
    
    private Vector3 playerMovement;
    private Quaternion playerRotation;

    private float gravity;

    public void SetForceRotation(bool forceRotation) {
        this.forceRotation = forceRotation;
    }

    public void Initialise(
        float moveSpeed,
        float jumpSpeed,
        float rotationSpeed,
        bool forceRotation,
        Animator animator,
        CharacterController characterController
        ) {
            this.moveSpeed = moveSpeed;
            this.jumpSpeed = jumpSpeed;
            this.rotationSpeed = rotationSpeed;
            this.forceRotation = forceRotation;
            this.animator = animator;
            this.characterController = characterController;

            playerRotation = characterController.transform.rotation;
        }

    public void UpdateMovement(Quaternion cameraRotation) {
        // Add gravity, but only while player is in the air
        gravity += Physics.gravity.y * Time.deltaTime;
        if (characterController.isGrounded) {
            gravity = -1f;

            // Let player jump
            if (Input.GetButtonDown("Jump")) {
                gravity = jumpSpeed;
            }
        }

        // Get input direction
        float horizontalInput = Input.GetAxisRaw("Horizontal");
        float verticalInput = Input.GetAxisRaw("Vertical");

        Vector3 inputDirection = new Vector3(horizontalInput, 0f, verticalInput);
        Quaternion cameraDirection = Quaternion.Euler(0f, cameraRotation.eulerAngles.y, 0f);

        if (forceRotation) {
            playerRotation = cameraDirection;
        }

        if (inputDirection.magnitude > 0.1f) {
            inputDirection = inputDirection.normalized;
            // Set head direction
            if (forceRotation) {
                playerMovement = moveSpeed * (cameraDirection * inputDirection);
            } else {
                playerRotation = Quaternion.Slerp(playerRotation, Quaternion.LookRotation(cameraDirection * inputDirection), rotationSpeed * Time.deltaTime);
                playerMovement = moveSpeed * (playerRotation * Vector3.forward);
            }

            animator.SetBool("isWalking", true);
        } else {
            playerMovement = Vector3.zero;
            animator.SetBool("isWalking", false);
        }

        characterController.transform.rotation = playerRotation;
        characterController.Move((playerMovement + new Vector3(0f, gravity, 0f)) * Time.deltaTime);
    }
}