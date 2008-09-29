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

import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 */
public class CrossValidationUtils
{
    public static final String CROSS_VALIDATION_STORAGE_KEY = CrossValidationStorage.class
            .getName();

    public static CrossValidationStorage getOrInitCrossValidationStorage()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext()
                .getRequestMap();

        if (!requestMap.containsKey(CROSS_VALIDATION_STORAGE_KEY))
        {
            resetCrossValidationStorage();
        }

        return (CrossValidationStorage) requestMap
                .get(CROSS_VALIDATION_STORAGE_KEY);
    }

    public static void resetCrossValidationStorage()
    {
        FacesContext
                .getCurrentInstance()
                .getExternalContext()
                .getRequestMap()
                .put(CROSS_VALIDATION_STORAGE_KEY, new CrossValidationStorage());
    }

    public static final String VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY = JsfUtils.class.getName();

    public static Map<String, ProcessedInformationEntry> getOrInitValueBindingConvertedValueMapping()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        if (!requestMap.containsKey(VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY))
        {
            resetValueBindingConvertedValueMapping();
        }

        return (Map<String, ProcessedInformationEntry>) requestMap.get(VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY);
    }

    public static void resetValueBindingConvertedValueMapping()
    {
        FacesContext.getCurrentInstance().getExternalContext().getRequestMap()
            .put(VALUE_BINDING_CONVERTED_VALUE_MAPPING_KEY, new HashMap<String, ProcessedInformationEntry>());
    }
}
