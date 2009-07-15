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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.crossval.storage.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.storage.ProcessedInformationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.storage.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.crossval.storage.ProcessedInformationStorage;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class CrossValidationUtils
{
    public static CrossValidationStorage getOrInitCrossValidationStorage()
    {
        return ExtValUtils.getStorage(CrossValidationStorage.class, CrossValidationStorage.class.getName());
    }

    public static void resetCrossValidationStorage()
    {
        ExtValUtils.resetStorage(CrossValidationStorage.class, CrossValidationStorage.class.getName());
    }

    public static ProcessedInformationStorage getOrInitProcessedInformationStorage()
    {
        return ExtValUtils.getStorage(
                ProcessedInformationStorage.class, ProcessedInformationStorage.class.getName());
    }

    public static void resetKeyToConvertedValueMapping()
    {
        ExtValUtils.resetStorage(ProcessedInformationStorage.class, ProcessedInformationStorage.class.getName());
    }

    public static ProcessedInformationStorageEntry resolveValidationTargetEntry(
            ProcessedInformationStorage processedInformationStorage,
            String targetKey, CrossValidationStorageEntry crossValidationStorageEntry)
    {
        ProcessedInformationStorageEntry processedInformationEntry =
            processedInformationStorage.getEntry(targetKey);

        //value not submitted at this request - use model value (validation against the model)
        if(processedInformationEntry == null)
        {
            return null;
        }

        //simple case
        if (processedInformationEntry.getFurtherEntries() == null)
        {
            return processedInformationEntry;
        }

        PropertyDetails propertyDetails = crossValidationStorageEntry.getMetaDataEntry()
                .getProperty(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        Object targetBean = propertyDetails.getBaseObject();

        //process complex component entries (e.g. a table)
        //supported: cross-component but no cross-entity validation (= locale validation)
        if (processedInformationEntry.getBean().equals(targetBean))
        {
            return processedInformationEntry;
        }

        for (ProcessedInformationStorageEntry entry : processedInformationEntry.getFurtherEntries())
        {
            if (entry.getBean().equals(targetBean))
            {
                return entry;
            }
        }

        return null;
    }

    @ToDo(value = Priority.MEDIUM, description = "support for map syntax")
    public static String convertValueBindingExpressionToProcessedInformationKey(ValueBindingExpression vbe)
    {
        return vbe.getExpressionString().replace("#{", "").replace("}", "");
    }
}
