{
  "$schema": "http://json-schema.org/draft-03/schema#",
  "description": "Adds a backup."
  "type": "object",
  "properties": {
    "action": {
      "type": "string",
      "enum": ["block", "allow"],
      "required": true
    },
    "direction": {
      "type": "string",
      "enum": ["inbound", "outbound"],
      "required": true
    },
    "protocol": {
      "type": "string",
      "enum": ["tcp", "udp", "icmp"],
      "required": true
    },
    "target": {
      "type": ["string", "object"]
    },
    "filter": {
      "type": ["array", "string", "object"]
    }
  }
}
