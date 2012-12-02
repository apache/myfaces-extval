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
 * Allows the extraction of {@link ValidationParameter}s.
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ValidationParameterExtractor
{
    /**
     * Extracts all {@link ValidationParameter}s hosted by the given annotation.
     * The wiki shows all supported styles.
     *
     * @param annotation The annotation which might contain validation-parameters
     * @return key/value map of the found parameters
     */
    Map<Object, List<Object>> extract(Annotation annotation);

    /**
     * Extracts all {@link ValidationParameter}s for the given key which are hosted by the given annotation.
     * The wiki shows all supported styles.
     *
     * @param annotation The annotation for which we want to extract the information.
     * @param key Value for the ParameterKey that is used for filtering the ValidationParameters.
     * @return list of the found parameters
     */
    List<Object> extract(Annotation annotation, Object key);

    /**
     * Extracts all {@link ValidationParameter}s for the given key which are hosted by the given annotation.
     * The result is filtered by the given type for the value.
     * The wiki shows all supported styles.
     *
     * @param annotation The annotation which might contain validation-parameters
     * @param key Value for the ParameterKey that is used for filtering the ValidationParameters.
     * @param valueType ParameterValue must be assignable to this type to be added to the result.
     * @param <T> Class type of the result which is the same as the class type of the parameter valueType.
     * @return list of the found parameters
     */
    <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType);

    /**
     * Extracts all {@link ValidationParameter}s for the given key which are hosted by the given annotation.
     * The result is filtered by the given type for the value and
     * the given id to receive a specific value if there are multiple parameters of the same type.
     * The wiki shows all supported styles.
     *
     * @param annotation The annotation which might contain validation-parameters
     * @param key Value for the ParameterKey that is used for filtering the ValidationParameters.
     * @param valueType ParameterValue must be assignable to this type to be added to the result.
     * @param valueId marker which identifies a specific value
     * (needed if there are multiple parameters of the same type)
     * @param <T> Class type of the result which is the same as the class type of the parameter valueType.
     * @return Value of the found ParameterValue.
     */
    <T> T extract(Annotation annotation, Object key, Class<T> valueType, Class valueId);
}