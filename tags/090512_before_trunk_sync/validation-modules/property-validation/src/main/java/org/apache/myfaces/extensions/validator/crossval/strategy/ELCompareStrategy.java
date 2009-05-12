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

import org.apache.myfaces.extensions.validator.crossval.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.CrossValidationUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import java.util.Map;

/**
 * referencing validation targets - possible formats:
 * "#{[bean_name].[property_name]}" ... cross-entity validation with value binding
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class ELCompareStrategy implements ReferencingStrategy
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean evaluateReferenceAndValidate(
            CrossValidationStorageEntry crossValidationStorageEntry,
            CrossValidationStorage crossValidationStorage,
            String validationTarget, AbstractCompareStrategy compareStrategy)
    {
        if (ExtValUtils.getELHelper().isELTermWellFormed(validationTarget) &&
            ExtValUtils.getELHelper().isELTermValid(FacesContext.getCurrentInstance(), validationTarget))
        {
            tryToValidateValueBinding(
                    crossValidationStorageEntry,
                    new ValueBindingExpression(validationTarget), crossValidationStorage, compareStrategy);

            return true;
        }
        return false;
    }

    @ToDo(value = Priority.MEDIUM, description = "test")
    protected boolean tryToValidateValueBinding(
            CrossValidationStorageEntry crossValidationStorageEntry,
            ValueBindingExpression validationTarget,
            CrossValidationStorage crossValidationStorage,
            AbstractCompareStrategy compareStrategy)
    {
        ProcessedInformationEntry validationTargetEntry =
                resolveTargetForCrossComponentValidation(crossValidationStorageEntry, validationTarget);

        if(validationTargetEntry != null)
        {
            processCrossComponentValidation(compareStrategy, crossValidationStorageEntry, validationTargetEntry);
        }
        else
        {
            processModelAwareCrossValidation(compareStrategy, crossValidationStorageEntry, validationTarget);
        }

        return true;
    }

    private ProcessedInformationEntry resolveTargetForCrossComponentValidation(
            CrossValidationStorageEntry crossValidationStorageEntry,
            ValueBindingExpression validationTarget)
    {
        Map<String, ProcessedInformationEntry> keyConvertedValueMapping =
                CrossValidationUtils.getOrInitKeyToConvertedValueMapping();

        return CrossValidationUtils.resolveValidationTargetEntry(
                keyConvertedValueMapping,
                CrossValidationUtils.convertValueBindingExpressionToProcessedInformationKey(validationTarget),
                crossValidationStorageEntry);
    }

    private void processCrossComponentValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry,
            ProcessedInformationEntry validationTargetEntry)
    {
        CrossValidationHelper
                .crossValidateCompareStrategy(
                        compareStrategy, crossValidationStorageEntry, validationTargetEntry, false);
    }

    private void processModelAwareCrossValidation(
            AbstractCompareStrategy compareStrategy,
            CrossValidationStorageEntry crossValidationStorageEntry,
            ValueBindingExpression validationTarget)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        Object targetValue = ExtValUtils.getELHelper().getValueOfExpression(facesContext, validationTarget);

        ProcessedInformationEntry targetEntry = new ProcessedInformationEntry();
        targetEntry.setBean(
                ExtValUtils.getELHelper().getValueOfExpression(facesContext, validationTarget.getBaseExpression()));
        targetEntry.setConvertedValue(targetValue);

        CrossValidationHelper
                .crossValidateCompareStrategy(compareStrategy, crossValidationStorageEntry, targetEntry, true);
    }
}
