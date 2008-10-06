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

import org.apache.myfaces.extensions.validator.crossval.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.referencing.strategy.AliasCompareStrategy;
import org.apache.myfaces.extensions.validator.crossval.referencing.strategy.ELCompareStrategy;
import org.apache.myfaces.extensions.validator.crossval.referencing.strategy.LocalCompareStrategy;
import org.apache.myfaces.extensions.validator.crossval.referencing.strategy.ReferencingStrategy;
import org.apache.myfaces.extensions.validator.crossval.referencing.strategy.LocalPropertyChainCompareStrategy;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInfo;

/**
 * @author Gerhard Petracek
 */
public abstract class AbstractCompareStrategy extends
        AbstractCrossValidationStrategy
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
                    .get(CustomInfo.BASE_PACKAGE) + "ReferencingStrategy";

            ReferencingStrategy customReferencingStrategy = (ReferencingStrategy) ClassUtils
                    .tryToInstantiateClassForName(customReferencingStrategyClassName);

            if (customReferencingStrategy != null)
            {
                referencingStrategies.add(customReferencingStrategy);
            }

            referencingStrategies.add(new ELCompareStrategy());
            referencingStrategies.add(new AliasCompareStrategy());
            referencingStrategies.add(new LocalCompareStrategy());
            referencingStrategies.add(new LocalPropertyChainCompareStrategy());
        }
    }

    public void processCrossValidation(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage)
            throws ValidatorException
    {

        initValidation(crossValidationStorageEntry);
        String[] validationTargets = getValidationTargets(crossValidationStorageEntry
                .getAnnotationEntry().getAnnotation());

        for (String validationTarget : validationTargets)
        {
            validationTarget = validationTarget.trim();

            //select validation method
            tryToValidate(crossValidationStorageEntry, crossValidationStorage,
                    validationTarget);
        }
    }

    private boolean tryToValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget)
    {
        for (ReferencingStrategy referencingStrategy : referencingStrategies)
        {
            if (referencingStrategy.evalReferenceAndValidate(
                    crossValidationStorageEntry, crossValidationStorage,
                    validationTarget, this))
            {
                return true;
            }
        }

        return false;
    }

    //has to be public for custom referencing strategies!!!
    public final void processTargetComponentAfterViolation(
            CrossValidationStorageEntry entryOfSource,
            CrossValidationStorageEntry entryOfTarget)
    {
        if (!handleTargetViolation(entryOfSource, entryOfTarget))
        {
            return;
        }

        FacesContext facesContext = FacesContext.getCurrentInstance();

        //get validation error messages for the target component
        String summary = getErrorMessageSummary(entryOfSource
                .getAnnotationEntry().getAnnotation(), true);
        String details = getErrorMessageDetails(entryOfSource
                .getAnnotationEntry().getAnnotation(), true);

        //validation target isn't bound to a component withing the current page 
        //(see validateFoundEntry, tryToValidateLocally and tryToValidateBindingOnly)
        if (entryOfTarget == null)
        {
            entryOfTarget = entryOfSource;
        }

        FacesMessage message;
        if (entryOfTarget.getAnnotationEntry() != null)
        {
            message = getTargetComponentErrorMessage(entryOfTarget
                    .getAnnotationEntry().getAnnotation(), summary, details);
        }
        else
        {
            //TODO document possible side effects
            //due to a missing target annotation (see: tryToValidateLocally)
            message = getTargetComponentErrorMessage(entryOfSource
                    .getAnnotationEntry().getAnnotation(), summary, details);
        }

        if (message.getSummary() != null || message.getDetail() != null)
        {
            facesContext.addMessage(entryOfTarget.getComponent().getClientId(
                    facesContext), message);
        }
    }

    //has to be public for custom referencing strategies!!!
    public final void processSourceComponentAfterViolation(
            CrossValidationStorageEntry entryOfSource)
    {
        if (!handleSourceViolation(entryOfSource))
        {
            return;
        }

        //get validation error messages for the current component
        String summary = getErrorMessageSummary(entryOfSource
                .getAnnotationEntry().getAnnotation(), false);
        String details = getErrorMessageDetails(entryOfSource
                .getAnnotationEntry().getAnnotation(), false);

        FacesMessage message = getSourceComponentErrorMessage(entryOfSource
                .getAnnotationEntry().getAnnotation(), summary, details);

        if (message.getSummary() != null || message.getDetail() != null)
        {
            //TODO
            throw new ValidatorException(message);
        }
        else
        {
            throw new ValidatorException(new FacesMessage());
        }
    }

    //has to be public for custom referencing strategies!!!
    public FacesMessage getSourceComponentErrorMessage(Annotation annotation,
            String summary, String details)
    {
        FacesMessage message = new FacesMessage();

        message.setSeverity(FacesMessage.SEVERITY_ERROR);
        message.setSummary(summary);
        message.setDetail(details);

        return message;
    }

    //has to be public for custom referencing strategies!!!
    public FacesMessage getTargetComponentErrorMessage(
            Annotation foundAnnotation, String summary, String details)
    {
        FacesMessage message = new FacesMessage();

        message.setSeverity(FacesMessage.SEVERITY_ERROR);
        message.setSummary(summary);
        message.setDetail(details);

        return message;
    }

    //has to be public for custom referencing strategies!!!
    public ProcessedInformationEntry resolveValidationTargetEntry(
            Map<String, ProcessedInformationEntry> valueBindingConvertedValueMapping,
            String targetValueBinding, Object bean)
    {
        ProcessedInformationEntry processedInformationEntry = valueBindingConvertedValueMapping
                .get(targetValueBinding);

        //simple case
        if (processedInformationEntry.getFurtherEntries() == null)
        {
            return processedInformationEntry;
        }

        //process complex component entries (e.g. a table)
        //supported: cross-component but no cross-entity validation (= locale validation)
        if (processedInformationEntry.getBean().equals(bean))
        {
            return processedInformationEntry;
        }

        for (ProcessedInformationEntry entry : processedInformationEntry
                .getFurtherEntries())
        {
            if (entry.getBean().equals(bean))
            {
                return entry;
            }
        }

        return null;
    }

    protected String getErrorMessageSummary(Annotation annotation,
            boolean isTargetComponent)
    {
        return resolveMessage(getValidationErrorMsgKey(annotation,
                isTargetComponent));
    }

    protected String getErrorMessageDetails(Annotation annotation,
            boolean isTargetComponent)
    {
        try
        {
            String key = getValidationErrorMsgKey(annotation, isTargetComponent);
            return (key != null) ? resolveMessage(key
                    + DETAIL_MESSAGE_KEY_POSTFIX) : null;
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

    /*
     * optional methods - recommended to override - have to be public for custom referencing strategies!!!
     */
    /**
     * the usage of this method requires a new instance
     * -> in case of validation strategy beans application/singleton isn't allowed
     */
    protected void initValidation(
            CrossValidationStorageEntry crossValidationStorageEntry)
    {
    }

    protected boolean handleTargetViolation(
            CrossValidationStorageEntry entryOfSource,
            CrossValidationStorageEntry entryOfTarget)
    {
        return true;
    }

    protected boolean handleSourceViolation(
            CrossValidationStorageEntry entryOfSource)
    {
        return true;
    }

    public boolean useTargetComponentToDisplayErrorMsg(
            CrossValidationStorageEntry crossValidationStorageEntry)
    {
        return handleTargetViolation(crossValidationStorageEntry, null);
    }

    /*
     * abstract methods
     */

    protected abstract String getValidationErrorMsgKey(Annotation annotation,
            boolean isTargetComponent);

    /*
     * implements the specific validation logic
     */

    public abstract boolean isViolation(Object object1, Object object2,
            Annotation annotation);

    /*
     * returns the referenced validation targets of the annotation
     * e.g. @DateIs(type = DateIsType.before, value = "finalExam")
     * -> method returns an array with one value ("finalExam")
     */
    public abstract String[] getValidationTargets(Annotation annotation);
}
