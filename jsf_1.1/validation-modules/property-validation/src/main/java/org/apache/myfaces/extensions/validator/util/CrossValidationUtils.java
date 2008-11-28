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

import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorage;
import org.apache.myfaces.extensions.validator.crossval.ProcessedInformationEntry;
import org.apache.myfaces.extensions.validator.crossval.CrossValidationStorageEntry;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;

import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class CrossValidationUtils
{
    public static final String CROSS_VALIDATION_STORAGE_KEY = CrossValidationStorage.class.getName();

    public static CrossValidationStorage getOrInitCrossValidationStorage()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        if (!requestMap.containsKey(CROSS_VALIDATION_STORAGE_KEY))
        {
            resetCrossValidationStorage();
        }

        return (CrossValidationStorage) requestMap.get(CROSS_VALIDATION_STORAGE_KEY);
    }

    public static void resetCrossValidationStorage()
    {
        FacesContext
                .getCurrentInstance()
                .getExternalContext()
                .getRequestMap()
                .put(CROSS_VALIDATION_STORAGE_KEY, new CrossValidationStorage());
    }

    public static final String KEY_TO_CONVERTED_VALUE_MAPPING_KEY = JsfUtils.class.getName();

    public static Map<String, ProcessedInformationEntry> getOrInitKeyToConvertedValueMapping()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        if (!requestMap.containsKey(KEY_TO_CONVERTED_VALUE_MAPPING_KEY))
        {
            resetKeyToConvertedValueMapping();
        }

        return (Map<String, ProcessedInformationEntry>) requestMap.get(KEY_TO_CONVERTED_VALUE_MAPPING_KEY);
    }

    public static void resetKeyToConvertedValueMapping()
    {
        FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
            .put(KEY_TO_CONVERTED_VALUE_MAPPING_KEY, new HashMap<String, ProcessedInformationEntry>());
    }

    public static ProcessedInformationEntry resolveValidationTargetEntry(
            Map<String, ProcessedInformationEntry> keyToConvertedValueMapping,
            String targetKey, CrossValidationStorageEntry crossValidationStorageEntry)
    {
        ProcessedInformationEntry processedInformationEntry =
            keyToConvertedValueMapping.get(targetKey);

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

        for (ProcessedInformationEntry entry : processedInformationEntry.getFurtherEntries())
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
