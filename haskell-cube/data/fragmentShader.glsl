#version 330

uniform vec4 diffuseColour;
uniform vec4 ambientColour;
uniform vec4 specularColour;
uniform float shininess;

uniform vec3 lightDirection;

uniform sampler2D diffuseMap;

in vec3 calculatedNormal;
in vec4 calculatedEye;
in vec4 rgba;
in vec2 fragmentUV;

out vec4 colorOut;

void main()
{
    vec4 spec = vec4(0.0);

    vec3 n = normalize(calculatedNormal);
    float intensity = max(dot(n,lightDirection), 0.0);

    if (intensity > 0.0)
    {
        vec3 e = normalize(vec3(calculatedEye));
        vec3 h = normalize(lightDirection + e);

        float intSpec = max(dot(h,n), 0.0);
        spec = specularColour * pow(intSpec,shininess);
    }

    vec4 texCol = texture(diffuseMap, fragmentUV);
    vec4 baseCol = mix(rgba, texCol, texCol.a);
    colorOut = baseCol * max(intensity * diffuseColour + spec, ambientColour);
}
