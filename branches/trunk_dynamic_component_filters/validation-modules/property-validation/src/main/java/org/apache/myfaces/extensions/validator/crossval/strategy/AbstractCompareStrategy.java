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
package org.apache.myfaces.extensions.validator.crossval.strategy;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public abstract class AbstractCompareStrategy extends AbstractCrossValidationStrategy
{
    protected static List<ReferencingStrategy> referencingStrategies;
    protected Map<Object, Object> violationResultStorage = new HashMap<Object, Object>();

    public AbstractCompareStrategy()
    {
        initReferencingStrategies();
    }

    protected void initReferencingStrategies()
    {
        if (referencingStrategies == null)
        {
            referencingStrategies = new ArrayList<ReferencingStrategy>();

            String customReferencingStrategyClassName =
                ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInformation.BASE_PACKAGE) + "ReferencingStrategy";

            ReferencingStrategy customReferencingStrategy = (ReferencingStrategy) ClassUtils
                    .tryToInstantiateClassForName(customReferencingStrategyClassName);

            if (customReferencingStrategy != null)
            {
                referencingStrategies.add(customReferencingStrategy);
            }

            referencingStrategies.add(new ELCompareStrategy());
            //referencingStrategies.add(new AliasCompareStrategy());
            referencingStrategies.add(new LocalCompareStrategy());
            referencingStrategies.add(new LocalPropertyChainCompareStrategy());
        }
    }

    public void processCrossValidation(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage) throws ValidatorException
    {
        //initCrossValidation is done in the CrossValidationPhaseListener

        String[] validationTargets = getValidationTargets(
            crossValidationStorageEntry.getMetaDataEntry().getValue(Annotation.class));

        for (String validationTarget : validationTargets)
        {
            validationTarget = validationTarget.trim();

            //select validation method
            tryToValidate(crossValidationStorageEntry, crossValidationStorage, validationTarget);
        }
    }

    private boolean tryToValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget)
    {
        for (ReferencingStrategy referencingStrategy : referencingStrategies)
        {
            if (referencingStrategy.evaluateReferenceAndValidate(
                    crossValidationStorageEntry, crossValidationStorage, validationTarget, this))
            {
                return true;
            }
        }

        return false;
    }

    @SuppressWarnings({"ThrowableInstanceNeverThrown"})
    protected final void processTargetComponentAfterViolation(
            CrossValidationStorageEntry entryOfSource,
            CrossValidationStorageEntry entryOfTarget)
    {
        if (!handleTargetViolation(entryOfSource, entryOfTarget))
        {
            //no target - because there is no target component - value was validated against the model
            if(entryOfTarget == null)
            {
                processTargetComponentAsSourceComponentAfterViolation(entryOfSource);
                return;
            }

            return;
        }

        FacesContext facesContext = FacesContext.getCurrentInstance();

        //get validation error messages for the target component
        String summary = getErrorMessageSummary(entryOfSource.getMetaDataEntry().getValue(Annotation.class), true);
        String details = getErrorMessageDetail(entryOfSource.getMetaDataEntry().getValue(Annotation.class), true);

        //validation target isn't bound to a component withing the current page 
        //(see validateFoundEntry, tryToValidateLocally and tryToValidateBindingOnly)
        if (entryOfTarget == null)
        {
            entryOfTarget = entryOfSource;
        }

        FacesMessage message;
        if (entryOfTarget.getMetaDataEntry() != null)
        {
            message = getTargetComponentErrorMessage(
                entryOfTarget.getMetaDataEntry().getValue(Annotation.class), summary, details);
        }
        else
        {
            //TODO document possible side effects
            //due to a missing target annotation (see: tryToValidateLocally)
            message = getTargetComponentErrorMessage(
                entryOfSource.getMetaDataEntry().getValue(Annotation.class), summary, details);
        }

        if ((message.getSummary() != null || message.getDetail() != null) &&
            entryOfSource.getClientId() != null && !entryOfSource.getClientId().equals(entryOfTarget.getClientId()))
        {
            ValidatorException validatorException = new ValidatorException(message);

            if(ExtValUtils.executeAfterThrowingInterceptors(
                    entryOfTarget.getComponent(), entryOfTarget.getMetaDataEntry(),
                    entryOfTarget.getConvertedObject(), validatorException, this))
            {
                facesContext.addMessage(entryOfTarget.getClientId(), validatorException.getFacesMessage());
            }
        }
    }

    private void processTargetComponentAsSourceComponentAfterViolation(CrossValidationStorageEntry entryOfSource)
    {
        //get validation error messages for the current component
        String summary = getReverseErrorMessageSummary(entryOfSource.getMetaDataEntry().getValue(Annotation.class));
        String details = getReverseErrorMessageDetail(entryOfSource.getMetaDataEntry().getValue(Annotation.class));

        FacesMessage message = getSourceComponentErrorMessage(
            entryOfSource.getMetaDataEntry().getValue(Annotation.class), summary, details);

        if (message.getSummary() != null || message.getDetail() != null)
        {
            //TODO
            throw new ValidatorException(message);
        }
        else
        {
            //TODO logging
        }
    }

    protected final void processSourceComponentAfterViolation(CrossValidationStorageEntry entryOfSource)
    {
        if (handleSourceViolation(entryOfSource))
        {
            //get validation error messages for the current component
            String summary = getErrorMessageSummary(entryOfSource.getMetaDataEntry().getValue(Annotation.class), false);
            String details = getErrorMessageDetail(entryOfSource.getMetaDataEntry().getValue(Annotation.class), false);

            FacesMessage message = getSourceComponentErrorMessage(
                entryOfSource.getMetaDataEntry().getValue(Annotation.class), summary, details);

            if (message.getSummary() != null || message.getDetail() != null)
            {
                //TODO
                throw new ValidatorException(message);
            }
        }

        //just throw a new message - the error message is at the target
        throw new ValidatorException(new FacesMessage());
    }

    protected FacesMessage getSourceComponentErrorMessage(Annotation annotation, String summary, String detail)
    {
        FacesMessage message = new FacesMessage();

        message.setSeverity(FacesMessage.SEVERITY_ERROR);
        message.setSummary(summary);
        message.setDetail(detail);

        return message;
    }

    protected FacesMessage getTargetComponentErrorMessage(Annotation foundAnnotation, String summary, String detail)
    {
        FacesMessage message = new FacesMessage();

        message.setSeverity(FacesMessage.SEVERITY_ERROR);
        message.setSummary(summary);
        message.setDetail(detail);

        return message;
    }

    protected String getErrorMessageSummary(Annotation annotation, boolean isTargetComponent)
    {
        return resolveMessage(getValidationErrorMsgKey(annotation, isTargetComponent));
    }

    protected String getErrorMessageDetail(Annotation annotation, boolean isTargetComponent)
    {
        try
        {
            String key = getValidationErrorMsgKey(annotation, isTargetComponent);
            return (key != null) ? resolveMessage(key + DETAIL_MESSAGE_KEY_POSTFIX) : null;
        }
        catch (MissingResourceException e)
        {
            if(logger.isWarnEnabled())
            {
                logger.warn("couldn't find key " + getValidationErrorMsgKey(annotation, isTargetComponent)
                    + DETAIL_MESSAGE_KEY_POSTFIX, e);
            }
        }
        return null;
    }

    protected String getValidationErrorMsgKey(Annotation annotation)
    {
        return getValidationErrorMsgKey(annotation, false);
    }

    protected boolean handleTargetViolation(
            CrossValidationStorageEntry entryOfSource,
            CrossValidationStorageEntry entryOfTarget)
    {
        return entryOfTarget != null && entryOfTarget.getComponent() != null;
    }

    protected boolean handleSourceViolation(CrossValidationStorageEntry entryOfSource)
    {
        return true;
    }

    protected boolean useTargetComponentToDisplayErrorMsg(CrossValidationStorageEntry crossValidationStorageEntry)
    {
        return handleTargetViolation(crossValidationStorageEntry, null);
    }

    /*
     * no target component (validation against the model) -> get reverse message for source component
     */
    protected String getReverseErrorMessageSummary(Annotation annotation)
    {
        //if the message is neutral
        return getErrorMessageSummary(annotation, true);
    }

    /*
     * no target component (validation against the model) -> get reverse message for source component
     */
    protected String getReverseErrorMessageDetail(Annotation annotation)
    {
        //if the message is neutral
        return getErrorMessageDetail(annotation, true);
    }

    /*
     * abstract methods
     */

    protected abstract String getValidationErrorMsgKey(Annotation annotation, boolean isTargetComponent);

    /*
     * implements the specific validation logic
     */

    public abstract boolean isViolation(Object object1, Object object2, Annotation annotation);

    /*
     * returns the referenced validation targets of the annotation
     * e.g. @DateIs(type = DateIsType.before, value = "finalExam")
     * -> method returns an array with one value ("finalExam")
     */
    public abstract String[] getValidationTargets(Annotation annotation);
}
