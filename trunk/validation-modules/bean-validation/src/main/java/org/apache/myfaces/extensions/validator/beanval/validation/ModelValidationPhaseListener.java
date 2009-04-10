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

import org.apache.myfaces.extensions.validator.beanval.ExtValBeanValidationContext;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.event.PhaseListener;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.validation.ConstraintViolation;
import javax.validation.ValidatorFactory;
import javax.validation.Validation;
import java.util.List;
import java.util.Set;
import java.util.Iterator;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
public class ModelValidationPhaseListener implements PhaseListener
{
    private static final long serialVersionUID = -3482233893186708878L;

    protected final Log logger = LogFactory.getLog(getClass());
    private ValidatorFactory validationFactory = Validation.buildDefaultValidatorFactory();

    public void afterPhase(PhaseEvent phaseEvent)
    {
        if (logger.isTraceEnabled())
        {
            logger.trace("jsr303 start model validation");
        }

        List<ModelValidationEntry> modelValidationEntries = ExtValBeanValidationContext.getCurrentInstance()
                .getAllModelValidationEntries(FacesContext.getCurrentInstance().getViewRoot().getViewId());

        List processedValidationTargets = new ArrayList();

        for(ModelValidationEntry modelValidationEntry : modelValidationEntries)
        {
            processModelValidation(modelValidationEntry, processedValidationTargets);
        }

        FacesContext.getCurrentInstance().renderResponse();
        
        if (logger.isTraceEnabled())
        {
            logger.trace("jsr303 validation finished");
        }
    }

    private void processModelValidation(ModelValidationEntry modelValidationEntry, List processedValidationTargets)
    {
        for(Object validationTarget : modelValidationEntry.getValidationTargets())
        {
            if(processedValidationTargets.contains(validationTarget) &&
                    !modelValidationEntry.getMetaData().displayInline())
            {
                continue;
            }

            if(!processedValidationTargets.contains(validationTarget))
            {
                processedValidationTargets.add(validationTarget);
            }

            Set<ConstraintViolation<Object>> violations = this.validationFactory.usingContext()
                    .messageInterpolator(ExtValBeanValidationContext.getCurrentInstance().getMessageInterpolator())
                    .getValidator()
                    .validate(validationTarget, modelValidationEntry.getGroups());

            if (violations != null && violations.size() > 0)
            {
                //jsf 2.0 is able to display multiple messages per component - so process all violations
                //jsf < 2.0 will just use the first one (it's only a little overhead)
                Iterator violationsIterator = violations.iterator();
                ConstraintViolation constraintViolation;
                while(violationsIterator.hasNext())
                {
                    constraintViolation = (ConstraintViolation)violationsIterator.next();
                    if(modelValidationEntry.getMetaData().displayInline())
                    {
                        processConstraintViolation(constraintViolation, modelValidationEntry, validationTarget, true);
                    }
                    else
                    {
                        processConstraintViolation(constraintViolation, modelValidationEntry, validationTarget, false);
                    }
                }
            }
        }
    }

    @SuppressWarnings({"ThrowableInstanceNeverThrown"})
    @ToDo(value = Priority.HIGH, description = "use ExtValUtils#createFacesMessage" +
            "check ExtValUtils#executeAfterThrowingInterceptors")
    private void processConstraintViolation(ConstraintViolation violation,
                                                  ModelValidationEntry modelValidationEntry,
                                                  Object validationTarget,
                                                  boolean displayAtComponent)
    {
        String violationMessage = violation.getMessage();

        ValidatorException validatorException = new ValidatorException(
                new FacesMessage(FacesMessage.SEVERITY_ERROR, violationMessage, violationMessage));

        FacesContext facesContext = FacesContext.getCurrentInstance();
        UIComponent uiComponent = null;
        String clientId = null;

        if(displayAtComponent)
        {
            uiComponent = modelValidationEntry.getComponent();
            clientId = modelValidationEntry.getComponent().getClientId(facesContext);
        }

        ExtValUtils.executeAfterThrowingInterceptors(
                uiComponent,
                null,
                validationTarget,
                validatorException,
                null);

        if (violationMessage.equals(validatorException.getFacesMessage().getSummary()) ||
                violationMessage.equals(validatorException.getFacesMessage().getDetail()))
        {
            facesContext.addMessage(clientId,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, violationMessage, violationMessage));
        }
        else
        {
            facesContext.addMessage(clientId, validatorException.getFacesMessage());
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