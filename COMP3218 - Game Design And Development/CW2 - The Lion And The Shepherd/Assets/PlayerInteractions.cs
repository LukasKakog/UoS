using System;
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;

public class PlayerInteractions {
    private List<String> playerInteractible; // All the interactible id's the player is allowed to interact with (interactibles may be locked for progression)
    
    private Transform interactionCentre;
    private float interactionRadius; // The radius of the overlapSphere
    private float alertRadius;

    private Camera playerCamera;
    private GameObject selectedObject;

    private List<Interactible> prevInteractibles; // Interactables in the vicinity last frame
    private List<Interactible> currentInteractibles; // Interactables in the vicinity this frame

    private LayerMask playerLayer;
    private LayerMask interactibleLayer;

    private int blueberriesCollected = 0;

    public void Initialise(
        Transform interactionCentre,
        float interactionRadius,
        float alertRadius,
        Camera playerCamera,
        LayerMask playerLayer,
        LayerMask interactibleLayer
    ) {
        this.interactionCentre = interactionCentre;
        this.interactionRadius = interactionRadius;
        this.alertRadius = alertRadius;
        this.playerCamera = playerCamera;
        this.playerLayer = playerLayer;
        this.interactibleLayer = interactibleLayer;

        prevInteractibles = new List<Interactible>();
        currentInteractibles = new List<Interactible>();

        playerInteractible = new List<string> {"Sheep"};
        selectedObject = null;
    }

    public List<string> GetInteractibles() {
        return playerInteractible;
    }

    public void SetInteractibles(List<string> interactibles) {
        playerInteractible = new List<string>(interactibles);
    }

    public void UpdateInteractibles() {
        currentInteractibles.Clear(); // Empty the current interactible list
        Collider[] currentColliders = Physics.OverlapSphere(interactionCentre.position, alertRadius, interactibleLayer);

        // Get all with interactible scripts
        foreach (Collider collider in currentColliders) {
            if (collider.GetComponent<Interactible>() != null) {
                currentInteractibles.Add(collider.GetComponent<Interactible>());
            } else if (collider.transform.parent.GetComponent<Interactible>() != null) {
                currentInteractibles.Add(collider.transform.parent.GetComponent<Interactible>());
            }
        }

        // Make all interactibles out of range invisible
        foreach (Interactible interactible in prevInteractibles) {
            if (currentInteractibles.Contains(interactible) == false) {
                interactible.visible = false;
            }
        }

        // Get the currently selected object
        GameObject closestObject = null;
        float objectScore = -math.INFINITY;

        foreach (Interactible interactible in currentInteractibles) {
            if (playerInteractible.Contains(interactible.id) && interactible.canInteract) {
                interactible.visible = true;
                interactible.UpdateAlert(playerCamera.transform.rotation);

                Vector3 closestPointToCamera = interactible.GetComponent<Collider>().ClosestPoint(playerCamera.transform.position);
                Vector3 closestPointToPlayer = interactible.GetComponent<Collider>().ClosestPoint(interactionCentre.position);

                float distanceFromCamera = Vector3.Distance(playerCamera.transform.position, closestPointToCamera);
                float distanceFromPlayer = Vector3.Distance(interactionCentre.position, closestPointToPlayer);
                float angleFromCamera = Vector3.Angle(closestPointToCamera - playerCamera.transform.position, playerCamera.transform.forward);
                float newScore = -distanceFromCamera - math.pow(angleFromCamera, 0.75f);

                if (angleFromCamera < 12 && distanceFromPlayer < interactionRadius && newScore > objectScore) {
                    closestObject = interactible.gameObject;
                    objectScore = newScore;
                }
            } else {
                interactible.visible = false;
            }
        }

        // Update prevInteractibles
        prevInteractibles = new List<Interactible>(currentInteractibles);

        // Selected material applying
        if (selectedObject != closestObject) {
            if (selectedObject != null) {
                Renderer[] selectedObjectRenderers = selectedObject.GetComponentsInChildren<Renderer>();
                foreach (Renderer renderer in selectedObjectRenderers) {
                    renderer.material.DisableKeyword("_EMISSION");
                }
            }

            if (closestObject != null) {
                Renderer[] closestObjectRenderers = closestObject.GetComponentsInChildren<Renderer>();
                foreach (Renderer renderer in closestObjectRenderers) {
                    renderer.material.EnableKeyword("_EMISSION");
                    renderer.material.SetColor("_EmissionColor", Color.gray);
                }
            }

            selectedObject = closestObject;
        }

        // Interact with interactible
        if (Input.GetKeyDown(KeyCode.E)) {
            Interact();
        }
    }

    public void Interact() {
        if (selectedObject == null) {
            return;
        }

        switch (selectedObject.GetComponent<Interactible>().id) {
            case "Blueberry":
                OnInteractBlueberry();
                break;
            case "2":
                Console.WriteLine("Tuesday");
                break;
        }
    }

    void OnInteractBlueberry() {
        foreach (Transform child in selectedObject.transform) {
            if (child.name == "Berry") {
                UnityEngine.Object.Destroy(child.gameObject);
            }
        }

        blueberriesCollected += 1;
        selectedObject.GetComponent<Interactible>().canInteract = false;
    }

    public int GetBlueberriesCollected() {
        return blueberriesCollected;
    }
}