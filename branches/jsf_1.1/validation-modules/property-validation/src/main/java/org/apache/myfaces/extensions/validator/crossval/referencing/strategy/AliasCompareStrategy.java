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
package org.apache.myfaces.extensions.validator.crossval.referencing.strategy;

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.annotation.TargetAlias;
import org.apache.myfaces.extensions.validator.crossval.annotation.extractor.
    DefaultValueBindingScanningAnnotationExtractor;
import org.apache.myfaces.extensions.validator.crossval.strategy.AbstractCompareStrategy;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import java.lang.reflect.Field;

/**
 * referencing validation targets - possible formats:
 * "#{[bean_name]}:@[alias_name]" ... cross-entity validation with @TargetAlias
 * or "@[alias_name]" ... global alias -> additional abstraction - doesn't depend on bean names, property names...
 * component which is annotated with the @TargetAlias has to be within the same page
 *
 * @author Gerhard Petracek
 */
public class AliasCompareStrategy implements ReferencingStrategy
{
    protected final Log logger = LogFactory.getLog(getClass());
    
    public boolean evalReferenceAndValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        if (validationTarget.startsWith("@"))
        {
            tryToValidateWithAlias(crossValidationStorageEntry,
                    crossValidationStorage, validationTarget.substring(1),
                    compareStrategy);
            return true;
        }
        else if (validationTarget.contains(":@*"))
        {
            if (validateBindingFormatWithAlias(validationTarget))
            {
                tryToValidateBindingWithAlias(crossValidationStorageEntry,
                        validationTarget, crossValidationStorage,
                        compareStrategy, false);
                return true;
            }
        }
        else if (validationTarget.contains(":@"))
        {
            if (validateBindingFormatWithAlias(validationTarget))
            {
                tryToValidateBindingWithAlias(crossValidationStorageEntry,
                        validationTarget, crossValidationStorage,
                        compareStrategy, true);
                return true;
            }
        }
        return false;
    }

    protected void tryToValidateWithAlias(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        boolean validationExecuted = false;

        boolean useModelValue = true;
        if (validationTarget.startsWith("*"))
        {
            useModelValue = false;
            validationTarget = validationTarget.substring(1);
        }

        //search for TargetAlias annotations
        for (CrossValidationStorageEntry entry : crossValidationStorage
                .getCrossValidationStorageEntries())
        {

            if (entry.getAnnotationEntry().getAnnotation() instanceof TargetAlias)
            {
                validationExecuted = validateTargetWithAlias(validationTarget,
                        crossValidationStorageEntry, entry, useModelValue,
                        compareStrategy);
            }

            if (validationExecuted)
            {
                break;
            }
        }
    }

    protected boolean tryToValidateBindingWithAlias(
            CrossValidationStorageEntry crossValidationStorageEntry,
            String targetProperty,
            CrossValidationStorage crossValidationStorage,
            AbstractCompareStrategy compareStrategy, boolean useModelValue)
    {
        String[] crossEntityReferenceWithBinding = extractCrossEntityReferenceWithBinding(targetProperty);

        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (!ExtValUtils.getELHelper().isExpressionValid(facesContext,crossEntityReferenceWithBinding[0]))
        {
            return false;
        }

        AnnotationExtractor extractor = new DefaultValueBindingScanningAnnotationExtractor();

        String alias;
        AnnotationEntry foundAnnotationEntry = null;

        int aliasStartIndex;

        if (useModelValue)
        {
            aliasStartIndex = crossEntityReferenceWithBinding[1].indexOf('@') + 1;
        }
        else
        {
            aliasStartIndex = crossEntityReferenceWithBinding[1].indexOf('@') + 2;
        }
        String targetAliasName = crossEntityReferenceWithBinding[1]
                .substring(aliasStartIndex);
        for (AnnotationEntry entry : extractor.extractAnnotations(facesContext,
                crossEntityReferenceWithBinding[0]))
        {
            if (entry.getAnnotation() instanceof TargetAlias)
            {
                alias = ((TargetAlias) entry.getAnnotation()).value();
                if (targetAliasName.equals(alias))
                {
                    foundAnnotationEntry = entry;
                    break;
                }
            }
        }

        if (foundAnnotationEntry == null)
        {
            return false;
        }

        Object referencedBean = null;
        Object validationTargetObject = null;
        if (useModelValue)
        {
            referencedBean = ExtValUtils.getELHelper().getValueOfExpression(facesContext,
                    crossEntityReferenceWithBinding[0]);
            validationTargetObject = getValidationTargetObject(
                    crossValidationStorageEntry, foundAnnotationEntry,
                    referencedBean, crossValidationStorage);
        }
        else
        {
            for (CrossValidationStorageEntry entry : crossValidationStorage
                    .getCrossValidationStorageEntries())
            {
                if (foundAnnotationEntry.getAnnotation() instanceof TargetAlias
                        && entry.getAnnotationEntry().getAnnotation() != null
                        && entry.getAnnotationEntry().getAnnotation() instanceof TargetAlias)
                {
                    if (((TargetAlias) foundAnnotationEntry.getAnnotation())
                            .value().equals(
                                    ((TargetAlias) entry.getAnnotationEntry()
                                            .getAnnotation()).value()))
                    {
                        validationTargetObject = entry.getConvertedObject();
                        break;
                    }
                }
            }
        }

        validateFoundEntry(crossValidationStorageEntry, foundAnnotationEntry,
                referencedBean, crossValidationStorage, validationTargetObject,
                compareStrategy);

        return true;
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    protected Object getValidationTargetObject(
            CrossValidationStorageEntry crossValidationStorageEntry,
            AnnotationEntry foundAnnotationEntry, Object referencedBean,
            CrossValidationStorage crossValidationStorage)
    {
        if (foundAnnotationEntry == null
                || foundAnnotationEntry.getBoundTo() == null
                || foundAnnotationEntry.getBoundTo().indexOf(":") < 0)
        {
            return null;
        }

        Object validationTargetObject = null;
        String boundTo = foundAnnotationEntry.getBoundTo();
        String name = boundTo.substring(boundTo.indexOf(":") + 1);
        if (boundTo.startsWith("[method]"))
        {
            String baseValueBindingExpression = foundAnnotationEntry
                    .getValueBindingExpression();

            String targetValueBindingExpression = baseValueBindingExpression
                    .substring(0, baseValueBindingExpression.length() - 1)
                    + "." + name + "}";
            FacesContext facesContext = FacesContext.getCurrentInstance();

            if (ExtValUtils.getELHelper().isExpressionValid(facesContext, targetValueBindingExpression))
            {
                validationTargetObject = ExtValUtils.getELHelper().getValueOfExpression(
                        facesContext, targetValueBindingExpression);
            }
        }
        else if (boundTo.startsWith("[field]"))
        {
            try
            {
                Field foundField;
                try
                {
                    foundField = referencedBean.getClass().getDeclaredField(
                            name);
                    foundField.setAccessible(true);
                    validationTargetObject = foundField.get(referencedBean);
                }
                catch (NoSuchFieldException e)
                {
                    foundField = referencedBean.getClass().getDeclaredField(
                            "_" + name);
                    foundField.setAccessible(true);
                    validationTargetObject = foundField.get(referencedBean);
                }
            }
            catch (Exception e)
            {
                if(logger.isWarnEnabled())
                {
                    logger.warn("couldn't access field " + name + " details: boundTo=" + boundTo, e);
                }
            }
        }

        return validationTargetObject;
    }

    protected void validateFoundEntry(
            CrossValidationStorageEntry crossValidationStorageEntry,
            AnnotationEntry foundAnnotationEntry, Object referencedBean,
            CrossValidationStorage crossValidationStorage,
            Object validationTargetObject,
            AbstractCompareStrategy compareStrategy)
    {
        boolean violationFound = false;

        if (compareStrategy.isViolation(crossValidationStorageEntry
                .getConvertedObject(), validationTargetObject,
                crossValidationStorageEntry.getAnnotationEntry()
                        .getAnnotation()))
        {
            //TODO use compareStrategy#useTargetComponentToDisplayErrorMsg
            compareStrategy.processTargetComponentAfterViolation(
                    crossValidationStorageEntry, null);

            violationFound = true;
        }

        if (violationFound)
        {
            compareStrategy
                    .processSourceComponentAfterViolation(crossValidationStorageEntry);
        }
    }

    protected boolean validateTargetWithAlias(String validationTarget,
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorageEntry entry, boolean useModelValue,
            AbstractCompareStrategy compareStrategy)
    {
        boolean validationExecuted = false;
        boolean violationFound = false;

        if (validationTarget.equals(((TargetAlias) entry.getAnnotationEntry()
                .getAnnotation()).value()))
        {
            //get an object to store all results

            Object validationTargetObject;

            if (useModelValue)
            {
                validationTargetObject = ExtValUtils.getELHelper().getValueOfExpression(
                        FacesContext.getCurrentInstance(), entry.getAnnotationEntry().getValueBindingExpression());
            }
            else
            {
                validationTargetObject = entry.getConvertedObject();
            }
            if (compareStrategy.isViolation(crossValidationStorageEntry
                    .getConvertedObject(), validationTargetObject,
                    crossValidationStorageEntry.getAnnotationEntry().getAnnotation()))
            {
                violationFound = true;

                compareStrategy.processTargetComponentAfterViolation(crossValidationStorageEntry, entry);
            }
            validationExecuted = true;
        }

        if (violationFound)
        {
            compareStrategy
                    .processSourceComponentAfterViolation(crossValidationStorageEntry);
        }

        return validationExecuted;
    }

    protected boolean validateBindingFormatWithAlias(String targetProperty)
    {
        int bindingStartIndex = targetProperty.indexOf("#{");
        int bindingEndIndex = targetProperty.indexOf("}");
        int separatorIndex = targetProperty.indexOf(":@");

        return (bindingStartIndex > -1 && bindingEndIndex > -1
                && separatorIndex > -1 && bindingStartIndex < bindingEndIndex && bindingEndIndex < separatorIndex);
    }

    protected String[] extractCrossEntityReferenceWithBinding(String targetProperty)
    {
        String[] result = new String[2];

        result[0] = targetProperty.substring(0, targetProperty.indexOf(":"));
        result[1] = targetProperty.substring(targetProperty.indexOf(":") + 1);

        return result;
    }
}
