/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.beanval.validation;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.beanval.BeanValidationModuleKey;
import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;
import org.apache.myfaces.extensions.validator.beanval.storage.ModelValidationEntry;
import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.validation.message.FacesMessageHolder;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.validation.ConstraintViolation;
import javax.validation.MessageInterpolator;
import javax.validation.Path;
import javax.validation.metadata.ConstraintDescriptor;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
public class ModelValidationPhaseListener implements PhaseListener
{
    private static final long serialVersionUID = -3482233893186708878L;

    protected final Log logger = LogFactory.getLog(getClass());

    public void afterPhase(PhaseEvent phaseEvent)
    {
        if (logger.isTraceEnabled())
        {
            logger.trace("jsr303 start model validation");
        }

        Map<Object, List<Class>> processedValidationTargets = new HashMap<Object, List<Class>>();

        Map<String, ModelValidationResult> results = new HashMap<String, ModelValidationResult>();

        for (ModelValidationEntry modelValidationEntry : getModelValidationEntriesToValidate())
        {
            processModelValidation(modelValidationEntry, processedValidationTargets, results);
        }

        processModelValidationResults(results);

        executeGlobalAfterValidationInterceptorsFor(results);

        if (logger.isTraceEnabled())
        {
            logger.trace("jsr303 validation finished");
        }
    }

    private List<ModelValidationEntry> getModelValidationEntriesToValidate()
    {
        return ExtValBeanValidationContext.getCurrentInstance().getModelValidationEntriesToValidate();
    }

    private void processModelValidation(ModelValidationEntry modelValidationEntry,
                                        Map<Object, List<Class>> processedValidationTargets,
                                        Map<String, ModelValidationResult> results)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        PropertyInformation propertyInformation;
        Set<ConstraintViolation<Object>> violations;
        Class[] groupsToValidate;

        for (Object validationTarget : modelValidationEntry.getValidationTargets())
        {
            propertyInformation = createPropertyInformation(modelValidationEntry, validationTarget);

            if (!executeGlobalBeforeValidationInterceptors(
                    facesContext, modelValidationEntry.getComponent(), validationTarget, propertyInformation))
            {
                return;
            }

            groupsToValidate = filterGroupsToValidate(
                    modelValidationEntry, validationTarget, processedValidationTargets);

            //TODO
            if (!addProcessedTarget(
                    modelValidationEntry, processedValidationTargets, validationTarget, groupsToValidate))
            {
                continue;
            }

            violations = validateTarget(validationTarget, groupsToValidate);

            if (violations != null && !violations.isEmpty())
            {
                processViolations(facesContext, modelValidationEntry, validationTarget, violations, results);
            }
        }
    }

    private Class[] filterGroupsToValidate(ModelValidationEntry modelValidationEntry,
                                           Object validationTarget,
                                           Map<Object, List<Class>> processedValidationTargets)
    {
        if(!processedValidationTargets.containsKey(validationTarget))
        {
            return modelValidationEntry.getGroups();
        }

        List<Class> result = new ArrayList<Class>();
        List<Class> validatedGroups = processedValidationTargets.get(validationTarget);

        for(Class group : modelValidationEntry.getGroups())
        {
            if(!validatedGroups.contains(group))
            {
                result.add(group);
            }
        }
        return result.toArray(new Class[result.size()]);
    }

    private boolean addProcessedTarget(ModelValidationEntry modelValidationEntry,
                                             Map<Object, List<Class>> processedValidationTargets,
                                             Object validationTarget,
                                             Class[] groups)
    {
        if (isTargetAlreadyProcessedForGroups(
                processedValidationTargets, validationTarget, groups) &&
                !modelValidationEntry.isDisplayMessageInline())
        {
            return false;
        }

        if (!isTargetAlreadyProcessedForGroups(
                processedValidationTargets, validationTarget, groups))
        {
            addTarget(processedValidationTargets, validationTarget, groups);
        }
        return true;
    }

    private boolean isTargetAlreadyProcessedForGroups(
            Map<Object, List<Class>> processedValidationTargets, Object validationTarget, Class[] groups)
    {
        List<Class> groupList;
        if(processedValidationTargets.containsKey(validationTarget))
        {
            groupList = processedValidationTargets.get(processedValidationTargets);

            for(Class group : groups)
            {
                if(!groupList.contains(group))
                {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private void addTarget(
            Map<Object, List<Class>> processedValidationTargets, Object validationTarget, Class[] groups)
    {
        if(!processedValidationTargets.containsKey(validationTarget))
        {
            processedValidationTargets.put(validationTarget, new ArrayList<Class>());
        }

        List<Class> validatedGroups = processedValidationTargets.get(validationTarget);

        for(Class group : groups)
        {
            if(!validatedGroups.contains(group))
            {
                validatedGroups.add(group);
            }
        }
    }

    private PropertyInformation createPropertyInformation(
            ModelValidationEntry modelValidationEntry, Object validationTarget)
    {
        PropertyInformation propertyInformation;
        PropertyDetails propertyDetails;
        propertyInformation = new DefaultPropertyInformation();
        if (modelValidationEntry.getComponent() != null)
        {
            propertyDetails = ExtValUtils.getELHelper()
                    .getPropertyDetailsOfValueBinding(modelValidationEntry.getComponent());
        }
        else
        {
            propertyDetails = new PropertyDetails(null, validationTarget, null);
        }
        propertyInformation.setInformation(PropertyInformationKeys.PROPERTY_DETAILS, propertyDetails);
        return propertyInformation;
    }

    private boolean executeGlobalBeforeValidationInterceptors(FacesContext facesContext,
                                                              UIComponent uiComponent,
                                                              Object validationTarget,
                                                              PropertyInformation propertyInformation)
    {
        return ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, validationTarget,
                PropertyInformation.class.getName(), propertyInformation, BeanValidationModuleKey.class);
    }

    private void executeGlobalAfterValidationInterceptors(FacesContext facesContext,
                                                          UIComponent uiComponent,
                                                          Object validationTarget,
                                                          PropertyInformation propertyInformation)
    {
        ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, validationTarget,
                PropertyInformation.class.getName(), propertyInformation, BeanValidationModuleKey.class);
    }

    private Set<ConstraintViolation<Object>> validateTarget(Object validationTarget, Class[] groups)
    {
        return ExtValBeanValidationContext.getCurrentInstance()
                .getValidatorFactory().usingContext()
                .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                .getValidator()
                .validate(validationTarget, groups);
    }

    private void processViolations(FacesContext facesContext,
                                   ModelValidationEntry modelValidationEntry,
                                   Object validationTarget,
                                   Set<ConstraintViolation<Object>> violations,
                                   Map<String, ModelValidationResult> results)
    {
        //jsf 2.0 is able to display multiple messages per component - so process all violations
        //jsf < 2.0 will just use the first one (it's only a little overhead)
        Iterator<ConstraintViolation<Object>> violationsIterator = violations.iterator();
        ConstraintViolation<Object> constraintViolation;
        ModelValidationResult result;
        while (violationsIterator.hasNext())
        {
            tryToCreateModelValidationResult(facesContext, modelValidationEntry, results);

            result = resolveModelValidationResult(facesContext, modelValidationEntry, results);

            constraintViolation = violationsIterator.next();
            addViolationMessage(modelValidationEntry, validationTarget, constraintViolation, result);
        }
    }

    private void tryToCreateModelValidationResult(FacesContext facesContext,
                                                  ModelValidationEntry modelValidationEntry,
                                                  Map<String, ModelValidationResult> results)
    {
        ModelValidationResult result;
        if (!isModelValidationResultAvailableFor(facesContext, modelValidationEntry, results))
        {
            result = new ModelValidationResult();
            results.put(modelValidationEntry.getComponent().getClientId(facesContext), result);
        }
    }

    private ModelValidationResult resolveModelValidationResult(FacesContext facesContext,
                                                               ModelValidationEntry modelValidationEntry,
                                                               Map<String, ModelValidationResult> results)
    {
        return results.get(modelValidationEntry.getComponent().getClientId(facesContext));
    }

    private boolean isModelValidationResultAvailableFor(FacesContext facesContext,
                                                        ModelValidationEntry modelValidationEntry,
                                                        Map<String, ModelValidationResult> results)
    {
        return results.containsKey(modelValidationEntry.getComponent().getClientId(facesContext));
    }

    private void addViolationMessage(ModelValidationEntry modelValidationEntry,
                                     Object validationTarget,
                                     ConstraintViolation<Object> constraintViolation,
                                     ModelValidationResult result)
    {
        if (modelValidationEntry.isDisplayMessageInline())
        {
            result.addFacesMessageHolder(createFacesMessageHolderForConstraintViolation(
                    constraintViolation, modelValidationEntry, validationTarget, true));
        }
        else
        {
            result.addFacesMessageHolder(createFacesMessageHolderForConstraintViolation(
                    constraintViolation, modelValidationEntry, validationTarget, false));
        }
    }

    private FacesMessageHolder createFacesMessageHolderForConstraintViolation(final ConstraintViolation violation,
                                                                              ModelValidationEntry modelValidationEntry,
                                                                              final Object validationTarget,
                                                                              boolean displayAtComponent)
    {
        final String newViolationMessage = tryToChangeViolationMessage(
                modelValidationEntry, validationTarget, violation);

        ConstraintViolation newConstraintViolation = new ConstraintViolation()
        {
            private ConstraintViolation wrapped = violation;

            public String getMessage()
            {
                return newViolationMessage;
            }

            public String getMessageTemplate()
            {
                return wrapped.getMessageTemplate();
            }

            public Object getRootBean()
            {
                return wrapped.getRootBean();
            }

            public Class getRootBeanClass()
            {
                return wrapped.getRootBeanClass();
            }

            public Object getLeafBean()
            {
                return wrapped.getLeafBean();
            }

            public Path getPropertyPath()
            {
                return wrapped.getPropertyPath();
            }

            public Object getInvalidValue()
            {
                return wrapped.getInvalidValue();
            }

            public ConstraintDescriptor getConstraintDescriptor()
            {
                return wrapped.getConstraintDescriptor();
            }
        };


        UIComponent uiComponent = null;
        String clientId = null;

        if (displayAtComponent)
        {
            uiComponent = modelValidationEntry.getComponent();
            clientId = uiComponent.getClientId(FacesContext.getCurrentInstance());
        }

        FacesMessageHolder result = new FacesMessageHolder(BeanValidationUtils
                .createFacesMessageForConstraintViolation(uiComponent, validationTarget, newConstraintViolation));
        result.setClientId(clientId);
        return result;
    }

    private String tryToChangeViolationMessage(ModelValidationEntry modelValidationEntry,
                                               Object validationTarget,
                                               ConstraintViolation violation)
    {
        if (!isDefaultMessage(modelValidationEntry))
        {
            return interpolateValidationErrorMessage(
                    modelValidationEntry.getCustomMessage(), validationTarget, violation);
        }
        return violation.getMessage();
    }

    private boolean isDefaultMessage(ModelValidationEntry modelValidationEntry)
    {
        return ModelValidation.DEFAULT_MESSAGE.equals(modelValidationEntry.getCustomMessage());
    }

    private String interpolateValidationErrorMessage(String extValInlineMessage,
                                                     final Object validationTarget, final ConstraintViolation violation)
    {
        return ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator()
                .interpolate(
                        extValInlineMessage,
                        new MessageInterpolator.Context()
                        {
                            public ConstraintDescriptor<?> getConstraintDescriptor()
                            {
                                return violation.getConstraintDescriptor();
                            }

                            public Object getValidatedValue()
                            {
                                return validationTarget;
                            }
                        }
                );
    }

    private void processModelValidationResults(Map<String, ModelValidationResult> results)
    {
        for (ModelValidationResult result : results.values())
        {
            BeanValidationUtils.processViolationMessages(result.getFacesMessageHolderList(), false);
        }
    }

    private void executeGlobalAfterValidationInterceptorsFor(Map<String, ModelValidationResult> results)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        UIComponent component;
        for (ModelValidationResult result : results.values())
        {
            for (FacesMessageHolder facesMessageHolder : result.getFacesMessageHolderList())
            {
                component = null;
                if (facesMessageHolder.getClientId() != null && !facesMessageHolder.getClientId().equals("*"))
                {
                    component = facesContext.getViewRoot().findComponent(facesMessageHolder.getClientId());
                }
                executeGlobalAfterValidationInterceptors(facesContext, component, null, null);
            }
        }
    }

    public void beforePhase(PhaseEvent phaseEvent)
    {
        //do nothing
    }

    public PhaseId getPhaseId()
    {
        return PhaseId.UPDATE_MODEL_VALUES;
    }
}
