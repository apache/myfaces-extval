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
package org.apache.myfaces.extensions.validator.internal;

/**
 * API:<br/>
 * parts you might need for custom implementations and which are quite stable in view of changes
 * <p/>
 * INTERNAL:<br/>
 * if you think about referencing an artifact which is marked as internal, ask for support.
 * there should be a better solution
 * <p/>
 * CUSTOMIZABLE:<br/>
 * a planned extension point which contains logic to customize the framework.
 * if it isn't also marked as API it might change in future releases.
 * however, we try to keep it as stable as possible and reasonable.
 * <p/>
 * REUSE:<br/>
 * an artifact which you can reuse for a custom artifact.
 * if it isn't marked as API it might change in future releases.
 * however, we try to keep it as stable as possible and reasonable.
 * <p/>
 * FALLBACK and ALTERNATIVE
 * e.g.: some mechanisms aren't deprecated, because it makes sense to use them.
 * however, it's not the default approach
 *
 * @author Gerhard Petracek
 */
public enum UsageEnum
{
    API,
    INTERNAL,
    CUSTOMIZABLE,
    REUSE,
    FALLBACK,
    ALTERNATIVE
}
