using UnityEngine;

public class PlayerCamera {
    // The mouse input for rotation
    private float xRotation;
    private float yRotation;

    // Mouse sensitivity for the camera
    private float xSensitivity;
    private float ySensitivity;

    // The camera, pivots and offsets for cameras
    private Camera camera;
    private Transform cameraPivot;

    private Vector3 firstPersonCameraOffset;
    private Vector3 thirdPersonCameraOffset;

    // The distance the camera must be from objects
    private float cameraDistanceFromPlayer;
    private float cameraDistanceFromObjects;

    // The camera position and rotation to be assigned to the camera
    private Vector3 cameraPosition;
    private Quaternion cameraRotation;

    // The player layermask
    private LayerMask playerLayer;

    // Option to force first person, and read if the camera is first person
    private bool forceFirstPersonCamera;
    private bool isFirstPerson;

    public void UpdateSensitivity(float xSensitivity, float ySensitivity) {
        this.xSensitivity = xSensitivity;
        this.ySensitivity = ySensitivity;
    }

    public Quaternion GetCameraRotation() {
        return cameraRotation;
    }

    public void SetCameraRotation(Quaternion cameraRotation) {
        this.cameraRotation = cameraRotation;
    }

    public bool IsFirstPerson() {
        return isFirstPerson;
    }

    public bool IsThirdPerson() {
        return !isFirstPerson;
    }

    public void Initialise(
        float xSensitivity,
        float ySensitivity,
        Camera camera,
        Transform cameraPivot,
        Vector3 firstPersonCameraOffset,
        Vector3 thirdPersonCameraOffset,
        float cameraDistanceFromPlayer,
        float cameraDistanceFromObjects,
        LayerMask playerLayer,
        bool forceFirstPersonCamera
    ) {
        this.xSensitivity = xSensitivity;
        this.ySensitivity = ySensitivity;

        this.camera = camera;
        this.cameraPivot = cameraPivot;

        this.firstPersonCameraOffset = firstPersonCameraOffset;
        this.thirdPersonCameraOffset = thirdPersonCameraOffset;

        this.cameraDistanceFromPlayer = cameraDistanceFromPlayer;
        this.cameraDistanceFromObjects = cameraDistanceFromObjects;

        this.playerLayer = playerLayer;
        this.forceFirstPersonCamera = forceFirstPersonCamera;

        xRotation = camera.transform.rotation.eulerAngles.y;
        yRotation = -camera.transform.rotation.eulerAngles.x;

        // Lock the mouse position
        Cursor.lockState = CursorLockMode.Locked;
        Cursor.visible = false;
    }

    public void UpdateCamera() {
        // Gets input direction
        float xInput = Input.GetAxisRaw("Mouse X");
        float yInput = Input.GetAxisRaw("Mouse Y");

        xRotation += xInput * xSensitivity;
        yRotation += yInput * ySensitivity;
        yRotation = Mathf.Clamp(yRotation, -75f, 75f);

        // Sets the camera rotation
        Quaternion rotation = Quaternion.Euler(-yRotation, xRotation, 0f);
        cameraRotation = rotation;

        if (forceFirstPersonCamera) {
            UpdateFirstPersonCamera();
        } else {
            UpdateThirdPersonCamera();
        }

        camera.transform.position = cameraPosition;
        camera.transform.rotation = cameraRotation;
    }

    private void UpdateThirdPersonCamera() {
        /*
        if (LevelController.GameState != "Playing") {
            Cursor.lockState = CursorLockMode.None;
            Cursor.visible = true;

            return;
        } else {
            Cursor.lockState = CursorLockMode.Locked;
            Cursor.visible = false;
        }
        */
        // Calculate the original third person camera position
        cameraPosition = cameraPivot.position + (cameraRotation * thirdPersonCameraOffset);

        // Calculate the direction and distance of the camera from the pivot
        Vector3 rayDirection = Vector3.Normalize(cameraPosition - cameraPivot.position);
        float rayDistance = Vector3.Magnitude(cameraPosition - cameraPivot.position);
        RaycastHit rayHitInfo;

        // Check if the original position is in/through a wall
        if (Physics.SphereCast(cameraPivot.position, cameraDistanceFromObjects, rayDirection, out rayHitInfo, rayDistance, ~playerLayer)) {
            cameraPosition = cameraPivot.position + (rayHitInfo.distance * rayDirection);

            // Check if the new position is in the player
            bool playerCollision = Physics.OverlapSphere(cameraPosition , cameraDistanceFromPlayer, playerLayer).Length > 0;
            if (playerCollision) {
                UpdateFirstPersonCamera();
                return;
            }
        }

        isFirstPerson = false;
    }

    private void UpdateFirstPersonCamera() {
        cameraPosition = cameraPivot.position + (cameraRotation * firstPersonCameraOffset);
        isFirstPerson = true;
    }
}