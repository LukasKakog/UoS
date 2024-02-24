using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour {
    // The scripts the player controller controls
    private PlayerMovement playerMovement;
    private PlayerCamera playerCamera;
    private PlayerInteractions playerInteractions;

    [Header("Camera Settings")]
    // Mouse sensitivity for the camera
    public float xCameraSensitivity = 1;
    public float yCameraSensitivity = 1;

    // The camera, pivots and offsets for cameras
    public Camera cameraObject;
    public Transform cameraPivot;

    public Vector3 firstPersonCameraOffset;
    public Vector3 thirdPersonCameraOffset;

    // The distance the camera must be from objects
    public float cameraDistanceFromPlayer;
    public float cameraDistanceFromObjects;

    [Header("Movement Settings")]
    public CharacterController characterController;

    public float movementSpeed = 5;
    public float jumpSpeed = 5;
    public float rotationSpeed = 1;

    public Animator playerAnimator;

    // Settings for how the player will interact with objects
    // The radiuses increase as you go down
    [Header("Interaction Settings")]
    public Transform interactionCentre;
    public float interactionRadius = 1; // The max distance you can be to interact with objects
    public float interactionAlertRadius = 5; // The distance where alerts start

    [Header("Layer Masks")]
    // The player layermask
    public LayerMask playerLayer;
    public LayerMask interactibleLayer;

    private Quaternion savedCameraRotation = Quaternion.identity;
    private List<string> savedPlayerInteractible;
    private bool inCutScene = false;

    // Start is called before the first frame update
    void Start() {
        playerCamera = new PlayerCamera();
        playerMovement = new PlayerMovement();
        playerInteractions = new PlayerInteractions();

        playerCamera.Initialise(
            xCameraSensitivity,
            yCameraSensitivity,
            cameraObject,
            cameraPivot,
            firstPersonCameraOffset,
            thirdPersonCameraOffset,
            cameraDistanceFromPlayer,
            cameraDistanceFromObjects,
            playerLayer,
            false);

        playerMovement.Initialise(
            movementSpeed,
            jumpSpeed,
            rotationSpeed,
            playerCamera.IsFirstPerson(),
            playerAnimator,
            characterController);

        playerInteractions.Initialise(
            interactionCentre,
            interactionRadius,
            interactionAlertRadius,
            cameraObject,
            playerLayer,
            interactibleLayer);
    }

    // Update is called once per frame
    void Update() {
        if (inCutScene) {
            playerInteractions.UpdateInteractibles();
            return;
        }

        playerMovement.SetForceRotation(playerCamera.IsFirstPerson());
        playerMovement.UpdateMovement(playerCamera.GetCameraRotation());

        playerInteractions.UpdateInteractibles();

        playerCamera.UpdateCamera();
    }

    public void OnCutSceneStart() {
        savedCameraRotation = playerCamera.GetCameraRotation();
        savedPlayerInteractible = playerInteractions.GetInteractibles();
        playerAnimator.enabled = false;
        inCutScene = true;

        playerInteractions.SetInteractibles(new List<string>());
    }

    public void OnCutSceneEnd() {
        playerCamera.SetCameraRotation(savedCameraRotation);
        playerInteractions.SetInteractibles(savedPlayerInteractible);
        playerAnimator.enabled = true;
        inCutScene = false;
    }

    public void SetPlayerPositionAndRotation(Vector3 position, Quaternion rotation) {
        gameObject.transform.position = position;
        gameObject.transform.rotation = rotation;
    }

    public void SetInteractibles(List<string> interactibles) {
        playerInteractions.SetInteractibles(interactibles);
    }

    public int GetBlueberriesCollected() {
        return playerInteractions.GetBlueberriesCollected();
    }
}