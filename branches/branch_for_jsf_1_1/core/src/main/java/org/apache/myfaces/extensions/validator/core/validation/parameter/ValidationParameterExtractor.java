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
package org.apache.myfaces.extensions.validator.core.validation.parameter;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.Map;
import java.util.List;
import java.lang.annotation.Annotation;

/**
 * Allows the extraction of ValidationParameters.
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ValidationParameterExtractor
{
    /**
     * Extract all the {@link ValidationParameter}s defined on the annotation. The resulting Map contains as key all
     * the {@link ParameterKey}s found.  The value for each key, is the value defined as the {@link ParameterValue}
     * (or the list of) of the ParameterValue part.<br/>
     * When no ValidationParameters found, the method need to return an empty Map.
     *
     * @param annotation The annotation for which we want to extract the information.
     * @return Map with all ParameterKeys and ParameterValues found.
     */
    Map<Object, List<Object>> extract(Annotation annotation);

    /**
     * Extract all the values defined as the {@link ParameterValue} found in the annotation for the key which is a
     * {@link ParameterKey}. When no values found, the method returns an empty list.
     *
     * @param annotation The annotation for which we want to extract the information.
     * @param key Value for the ParameterKey that is used for filtering the ValidationParameters found.
     * @return List with all the ParameterValues found.
     */
    List<Object> extract(Annotation annotation, Object key);

    /**
     * Extract all the values defined as the {@link ParameterValue} found in the annotation for the key which is a
     * {@link ParameterKey} where only those results are included which are assignable to the type specified in the
     * parameter valueType. When no values found, the method returns an empty list.
     *
     * @param annotation The annotation for which we want to extract the information.
     * @param key Value for the ParameterKey that is used for filtering the ValidationParameters found.
     * @param valueType ParameterValue must be assignable to this type to be added to the result.
     * @param <T> Class type of the result which is the same as the class type of the parameter valueType.
     * @return List with all the ParameterValues found.
     */
    <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType);

    /**
     * Extract the value defined as the {@link ParameterValue} found in the annotation for the key which is a
     * {@link ParameterKey} where only those results are included which are assignable to the type specified in the
     * parameter valueType and the ParameterValue id defined is the same as the parameter valueId. <br/>
     * Multiple ParameterValues can be defined and each of them could be assigned an id to distinguish the different
     * values. <br/>
     * When no value matches the parameters of the method, the result of the method invocation is null.
     *
     * @param annotation The annotation for which we want to extract the information.
     * @param key Value for the ParameterKey that is used for filtering the ValidationParameters found.
     * @param valueType ParameterValue must be assignable to this type to be added to the result.
     * @param valueId value of the id member of the ParameterValue we are looking for.
     * @param <T> Class type of the result which is the same as the class type of the parameter valueType.
     * @return Value of the found ParameterValue.
     */
    <T> T extract(Annotation annotation, Object key, Class<T> valueType, Class valueId);
}
