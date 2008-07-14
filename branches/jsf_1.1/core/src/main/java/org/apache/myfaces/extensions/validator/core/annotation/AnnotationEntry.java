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
package org.apache.myfaces.extensions.validator.core.annotation;

import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 */
public class AnnotationEntry {
    private Annotation annotation;
    private String valueBindingExpression;
    //e.g. valueBindingName, DefaultValueBindingScanningAnnotationExtractor uses it for the variable-/method-name
    private String boundTo;
    private Class entityClass;

    //requested by bernhard huemer
    public <T> T getAnnotation(Class<T> targetClass) {
        return (T) annotation;
    }

    /*
     * generated
     */
    public Annotation getAnnotation() {
        return annotation;
    }

    public void setAnnotation(Annotation annotation) {
        this.annotation = annotation;
    }

    public String getValueBindingExpression() {
        return valueBindingExpression;
    }

    public void setValueBindingExpression(String valueBindingExpression) {
        this.valueBindingExpression = valueBindingExpression;
    }

    public String getBoundTo() {
        return boundTo;
    }

    public void setBoundTo(String boundTo) {
        this.boundTo = boundTo;
    }

    public Class getEntityClass() {
        return entityClass;
    }

    public void setEntityClass(Class entityClass) {
        this.entityClass = entityClass;
    }
}
